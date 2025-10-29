# RUNNING: multi-pair FAST v3.5 (DST-tolerant Auto-REPAIR + Resume, CT full-clock)
# - Processes many pairs; up to MAX_PER_PAIR chunks each (repairs first, then backfill).
# - Skips a pair if its earliest file already reaches STOP_BOUNDARY (2015-01-01 CT).
# - Keeps windows that are within ±60 min of ideal due to DST/early-close quirks.
# - Detects real gaps/overlaps and repairs them with exact CT windows.
# - Each saved CSV = EXACT DAYS_PER_CHUNK CT days (16:00 → 15:59).
# - Next end = previous start - 1 minute (backward) or prev_end + window (forward repair).
# - Global daily cap prevents hitting provider limits.

import os, re, time
from datetime import datetime, timedelta
from pathlib import Path
from typing import Optional, Tuple, List

import pandas as pd
from dotenv import load_dotenv
from webdriver_manager.chrome import ChromeDriverManager
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains
from selenium.common.exceptions import ElementClickInterceptedException, StaleElementReferenceException

# ---------- Pair set (45 base pairs sufficient for 120 triangles) ----------
PAIRS = [
    # USD base vs other G10 (9)
    "^USDEUR", "^USDGBP", "^USDAUD", "^USDNZD", "^USDCAD", "^USDCHF", "^USDJPY", "^USDSEK", "^USDNOK",

    # EUR base vs other G10 (9) — excluding USD duplicates but keeping GBP
    "^EURUSD", "^EURGBP", "^EURAUD", "^EURNZD", "^EURCAD", "^EURCHF", "^EURJPY", "^EURSEK", "^EURNOK",

    # GBP base vs other G10 (9) — excluding USD/EUR duplicates
    "^GBPUSD", "^GBPEUR", "^GBPAUD", "^GBPNZD", "^GBPCAD", "^GBPCHF", "^GBPJPY", "^GBPSEK", "^GBPNOK"
]

# ---------- Config ----------
MAX_PER_PAIR = 9                  # at most 5 chunks per pair per run
GLOBAL_MAX_DOWNLOADS = 250        # global daily cap (change if needed)
DEFAULT_END_DATE = "2025-08-07"   # used if no files exist (calendar date)
DAYS_PER_CHUNK = 17               # safe under 20k with weekends included
TARGET_WINDOW_MIN = DAYS_PER_CHUNK * 1440
DST_TOLERANCE_MIN = 60            # allow ±60 minutes for DST/early-close
STOP_BOUNDARY = datetime(2015, 1, 1)  # stop once current_end < this date (CT boundary)

# CT session alignment (FX day ends 15:59 CT; next day starts 16:00 CT)
SESSION_TZ = "America/Chicago"
SESSION_END_H = 15
SESSION_END_M = 59

DOWNLOAD_DIR = Path("barchart_downloads")
DOWNLOAD_DIR.mkdir(exist_ok=True)

MAX_RETRIES_PER_END = 2            # retry budget for micro-chunks
MIN_COVERAGE_RATIO = 0.50          # minimal fraction of expected minutes to accept

# ---------- Credentials ----------
load_dotenv()
EMAIL = os.getenv("BARCHART_EMAIL")
PASSWORD = os.getenv("BARCHART_PASSWORD")
if not EMAIL or not PASSWORD:
    raise SystemExit("Missing BARCHART_EMAIL or BARCHART_PASSWORD in .env")

# ---------- Driver ----------
opts = webdriver.ChromeOptions()
prefs = {
    "download.default_directory": str(DOWNLOAD_DIR.resolve()),
    "download.prompt_for_download": False,
    "download.directory_upgrade": True,
    "safebrowsing.enabled": True,
}
opts.add_experimental_option("prefs", prefs)
# opts.add_argument("--headless=new")  # optional
opts.add_argument("--start-maximized")
opts.add_argument("--blink-settings=imagesEnabled=false")
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=opts)

# ---------- tiny helpers ----------
def click(locator, to=10):
    WebDriverWait(driver, to).until(EC.element_to_be_clickable(locator)).click()

def find(locator, to=12):
    return WebDriverWait(driver, to).until(EC.presence_of_element_located(locator))

def maybe_click(locator, to=4):
    try:
        click(locator, to); return True
    except:
        return False

def dismiss_overlays(max_tries=3):
    for _ in range(max_tries):
        try:
            for sel in [
                ".ad-block-modal.in .close",
                ".ad-block-modal .close",
                ".reveal-modal.in .close",
                ".reveal-modal .close",
                "[data-dismiss='modal']",
                "button[aria-label='Close']",
                ".modal .close",
                ".bc-fdn-modal .close",
                ".bc-fdn-modal [data-dismiss='modal']",
            ]:
                for el in driver.find_elements(By.CSS_SELECTOR, sel):
                    try: driver.execute_script("arguments[0].click();", el)
                    except: pass
            driver.execute_script("""
                const hide = sel => document.querySelectorAll(sel).forEach(n=>{
                    n.style.display='none'; n.style.visibility='hidden'; n.setAttribute('aria-hidden','true');
                });
                hide('.ad-block-modal'); hide('.reveal-modal.in'); hide('.bc-fdn-modal');
                hide('.modal-backdrop'); hide('.reveal-modal');
            """)
            ActionChains(driver).send_keys(Keys.ESCAPE).pause(0.05).perform()
            time.sleep(0.1)
        except:
            pass

def switch_to_download_frame():
    driver.switch_to.default_content()
    for fr in driver.find_elements(By.TAG_NAME, "iframe"):
        try:
            driver.switch_to.frame(fr)
            if driver.find_elements(By.CSS_SELECTOR, "a.download-btn, button.download-btn"):
                return True
        except:
            pass
        driver.switch_to.default_content()
    return False

def ensure_in_download_context():
    if driver.find_elements(By.CSS_SELECTOR, "a.download-btn, button.download-btn"):
        return
    if not switch_to_download_frame():
        driver.switch_to.default_content()

def _download_container():
    ensure_in_download_context()
    for loc in [
        (By.NAME, "dateFrom"),
        (By.NAME, "dateTo"),
        (By.CSS_SELECTOR, "select[data-ng-model='frequency']"),
        (By.CSS_SELECTOR, "a.reset-btn"),
        (By.XPATH, "//a[contains(@class,'download-btn') and normalize-space()='Download']"),
        (By.XPATH, "//button[contains(@class,'download-btn') and normalize-space()='Download']"),
    ]:
        try:
            node = WebDriverWait(driver, 8).until(EC.presence_of_element_located(loc))
            return node.find_element(By.XPATH, "ancestor::*[self::section or self::div][1]")
        except:
            continue
    return driver

def _get_form_container():
    ensure_in_download_context()
    date_from = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.NAME, "dateFrom")))
    return date_from.find_element(By.XPATH, "ancestor::*[self::section or self::div][1]")

def _find_symbol_host_or_die():
    form = _get_form_container()
    try:
        wrapper = form.find_element(By.CSS_SELECTOR, ".price-history-symbol-selector")
        return wrapper.find_element(By.CSS_SELECTOR, "bc-symbol-search")
    except:
        pass
    try:
        return form.find_element(By.CSS_SELECTOR, "bc-symbol-search")
    except:
        host = driver.execute_script("return document.querySelector('.price-history-symbol-selector bc-symbol-search') || document.querySelector('bc-symbol-search');")
        if host:
            return host
        raise RuntimeError("Can't find bc-symbol-search host for symbol field.")

def fill_symbol(symbol):
    host = _find_symbol_host_or_die()
    driver.execute_script("arguments[0].scrollIntoView({block:'center'});", host)
    time.sleep(0.1)
    try: driver.execute_script("arguments[0].focus && arguments[0].focus();", host)
    except: pass
    actions = ActionChains(driver)
    actions.move_to_element_with_offset(host, 8, 8).click().pause(0.12)
    actions.key_down(Keys.CONTROL).send_keys("a").key_up(Keys.CONTROL)
    actions.send_keys(symbol).pause(0.4).send_keys(Keys.ENTER).perform()
    time.sleep(0.3)

def clamp_date_for_input(el, dt):
    min_attr = el.get_attribute("data-min-date")
    max_attr = el.get_attribute("data-max-date")
    d = dt
    if min_attr:
        try: d = max(d, datetime.strptime(min_attr, "%Y-%m-%d"))
        except: pass
    if max_attr:
        try: d = min(d, datetime.strptime(max_attr, "%Y-%m-%d"))
        except: pass
    return d

def set_input_value_js(el, mmddyyyy):
    driver.execute_script("""
        const el = arguments[0], val = arguments[1];
        el.value = val;
        el.dispatchEvent(new Event('input', {bubbles:true}));
        el.dispatchEvent(new Event('change', {bubbles:true}));
    """, el, mmddyyyy)

def set_end_only(end_dt):
    ensure_in_download_context()
    e_in = find((By.NAME, "dateTo"), to=10)
    e_dt = clamp_date_for_input(e_in, end_dt)
    e_txt = e_dt.strftime("%m/%d/%Y")
    set_input_value_js(e_in, e_txt)
    try:
        e_in.click(); time.sleep(0.05)
        ActionChains(driver).send_keys(Keys.ENTER).pause(0.05).perform()
        try:
            driver.execute_script("arguments[0].blur();", e_in)
        except StaleElementReferenceException:
            e_in2 = find((By.NAME, "dateTo"), to=6)
            driver.execute_script("arguments[0].blur();", e_in2)
    except:
        pass
    time.sleep(0.25)

def set_intraday_1min():
    ensure_in_download_context()
    sel = find((By.CSS_SELECTOR, "select[data-ng-model='frequency']"))
    Select(sel).select_by_value("string:minutes")
    mins = find((By.CSS_SELECTOR, "input[name='aggregation'].js-chart-form-minutes"))
    mins.click(); mins.send_keys(Keys.CONTROL, "a"); mins.send_keys("1"); mins.send_keys(Keys.TAB)

def _visible(el):
    try: return el.is_displayed()
    except: return False

def _distance_between(el1, el2):
    try:
        r1 = el1.rect; r2 = el2.rect
        c1x, c1y = r1['x'] + r1['width']/2, r1['y'] + r1['height']/2
        c2x, c2y = r2['x'] + r2['width']/2, r2['y'] + r2['height']/2
        dx, dy = c1x - c2x, c1y - c2y
        return (dx*dx + dy*dy) ** 0.5
    except:
        return 1e12

def get_correct_download_button(timeout=14):
    t0 = time.time(); last_err = None
    while time.time() - t0 < timeout:
        try:
            root = _download_container()
            for sel in [
                "a.download-btn[data-api-config]",
                "a.download-btn[data-bc-download-button]",
                "a.download-btn",
                "button.download-btn",
            ]:
                try:
                    cand = (root if root is not driver else driver).find_element(By.CSS_SELECTOR, sel)
                    if _visible(cand): return cand
                except Exception as e:
                    last_err = e
            date_from = find((By.NAME, "dateFrom"), to=4)
            candidates = [c for c in driver.find_elements(By.CSS_SELECTOR, "a.download-btn, button.download-btn") if _visible(c)]
            if candidates:
                return min(candidates, key=lambda c: _distance_between(date_from, c))
        except Exception as e:
            last_err = e
        time.sleep(0.2)
    raise TimeoutError(f"Couldn't find Download button: {last_err}")

def wait_api_config(symbol, expect_type='minutes', timeout=12):
    target1 = symbol; target2 = symbol.lstrip("^")
    t0 = time.time()
    while time.time() - t0 < timeout:
        btn = get_correct_download_button(timeout=3)
        cfg = btn.get_attribute("data-api-config")
        if not cfg: return btn
        if (target1 in cfg or target2 in cfg) and (expect_type in cfg): return btn
        time.sleep(0.25)
    return get_correct_download_button(timeout=3)

def wait_for_download(timeout=180) -> Optional[Path]:
    start = time.time()
    before = {f.name for f in DOWNLOAD_DIR.iterdir()}
    while time.time() - start < timeout:
        time.sleep(0.5)
        now = {f.name for f in DOWNLOAD_DIR.iterdir()}
        if any(n.endswith(".crdownload") for n in now): continue
        new = [n for n in now - before if n.lower().endswith(".csv")]
        if new:
            files = [DOWNLOAD_DIR / n for n in now if n.lower().endswith(".csv")]
            return max(files, key=lambda p: p.stat().st_mtime)
    return None

# ---------- CT window helpers ----------
def _ct_window_for_end_date(end_cap_dt: datetime) -> Tuple[pd.Timestamp, pd.Timestamp, int]:
    end_ct = pd.Timestamp(end_cap_dt.year, end_cap_dt.month, end_cap_dt.day,
                          SESSION_END_H, SESSION_END_M, tz=SESSION_TZ)
    start_ct = end_ct - pd.Timedelta(days=DAYS_PER_CHUNK) + pd.Timedelta(minutes=1)
    expected_minutes = int((end_ct - start_ct) / pd.Timedelta(minutes=1)) + 1
    return start_ct, end_ct, expected_minutes

# SNAP filenames to intended CT window regardless of first/last row
def trim_to_fixed_window(csv_path, end_cap_dt: datetime) -> Tuple[datetime, datetime, pd.DataFrame, int]:
    df = pd.read_csv(csv_path)
    tcol = next((c for c in df.columns if any(k in c.lower() for k in ("time","date","timestamp"))), df.columns[0])

    ts = pd.to_datetime(df[tcol].astype(str), format="%m/%d/%Y %H:%M", errors="coerce")
    if ts.isna().all():
        ts = pd.to_datetime(df[tcol].astype(str), errors="coerce")

    mask = ~ts.isna()
    df = df.loc[mask].copy(); ts = ts[mask]
    if df.empty:
        raise ValueError("No parsable datetimes after cleaning")

    ts_ct = ts.dt.tz_localize(SESSION_TZ, nonexistent="shift_forward", ambiguous="NaT").dropna()
    df["_ts_ct"] = ts_ct
    df.sort_values("_ts_ct", inplace=True)

    start_ct, end_ct, expected_minutes = _ct_window_for_end_date(end_cap_dt)

    df = df[(df["_ts_ct"] >= start_ct) & (df["_ts_ct"] <= end_ct)]
    if df.empty:
        raise ValueError("Alignment produced empty slice")

    df["_ts_ct_naive"] = df["_ts_ct"].dt.tz_convert(SESSION_TZ).dt.tz_localize(None)
    df[tcol] = df["_ts_ct_naive"].dt.strftime("%m/%d/%Y %H:%M")
    df = df.drop(columns=["_ts_ct", "_ts_ct_naive"])

    # File name bounds = intended CT window
    trim_oldest = start_ct.tz_convert(SESSION_TZ).tz_localize(None)
    trim_newest = end_ct.tz_convert(SESSION_TZ).tz_localize(None)

    return trim_oldest, trim_newest, df, expected_minutes

# ---------- File scan / planning ----------
FNAME_RE = re.compile(r"^([A-Z]{6,})_(\d{12})-(\d{12})\.csv$", re.I)
def parse_fname(name: str):
    m = FNAME_RE.match(name)
    if not m: return None
    sym, s, e = m.group(1), m.group(2), m.group(3)
    try:
        sdt = datetime.strptime(s, "%Y%m%d%H%M")
        edt = datetime.strptime(e, "%Y%m%d%H%M")
        return sym.upper(), sdt, edt
    except:
        return None

def _duration_minutes(a: datetime, b: datetime) -> int:
    return int((b - a).total_seconds() // 60) + 1

def _is_ct_aligned_window(sdt: datetime, edt: datetime) -> bool:
    """
    DST-tolerant alignment check:
      - Duration must be within ±DST_TOLERANCE_MIN of TARGET_WINDOW_MIN.
      - End must be 15:59 CT (we name using this).
      - Start may deviate ≤ DST_TOLERANCE_MIN from ideal.
    """
    dur = _duration_minutes(sdt, edt)
    if abs(dur - TARGET_WINDOW_MIN) > DST_TOLERANCE_MIN:
        return False
    if not (edt.hour == 15 and edt.minute == 59):
        return False
    ideal_start = edt - timedelta(minutes=TARGET_WINDOW_MIN) + timedelta(minutes=1)
    start_off = abs(int((sdt - ideal_start).total_seconds() // 60))
    return start_off <= DST_TOLERANCE_MIN

def scan_aligned_files(pair: str, folder: Path) -> Tuple[List[Tuple[datetime, datetime, Path]], List[Path]]:
    prefix = pair.lstrip("^").upper() + "_"
    keep: List[Tuple[datetime, datetime, Path]] = []
    misaligned: List[Path] = []
    for p in folder.glob("*.csv"):
        nm = p.name
        if "raw" in nm.lower():
            continue
        if not nm.upper().startswith(prefix):
            continue
        parsed = parse_fname(nm)
        if not parsed:
            continue
        _, sdt, edt = parsed
        if _is_ct_aligned_window(sdt, edt):
            keep.append((sdt, edt, p))
        else:
            misaligned.append(p)
    keep.sort(key=lambda t: t[0])  # ascending by start
    return keep, misaligned

def plan_repairs_and_deletes(files: List[Tuple[datetime, datetime, Path]]) -> Tuple[List[datetime], List[Path]]:
    """
    Returns:
      - list of repair END dates (each yields one aligned chunk),
      - list of files to delete first (true overlaps).
    Tolerates small gap/overlap within ±DST_TOLERANCE_MIN (DST spring/fall artifacts).
    """
    repairs: List[datetime] = []
    deletes: List[Path] = []
    tm = timedelta(minutes=TARGET_WINDOW_MIN)

    for i in range(len(files) - 1):
        s1, e1, _p1 = files[i]
        s2, e2, p2 = files[i+1]

        expected_next_start = e1 + timedelta(minutes=1)
        gap_min = int((s2 - expected_next_start).total_seconds() // 60)

        # TOLERATE small gap/overlap within ±DST_TOLERANCE_MIN
        if abs(gap_min) <= DST_TOLERANCE_MIN:
            continue

        if s2 <= e1:
            # true overlap beyond tolerance → delete next and insert one correct window
            deletes.append(p2)
            repairs.append(e1 + tm)
        else:
            # real gap → add as many windows as needed
            cur_end = e1
            while True:
                need_start = cur_end + timedelta(minutes=1)
                if need_start >= s2:
                    break
                repairs.append(cur_end + tm)
                cur_end = cur_end + tm
    return repairs, deletes

def find_backward_end(files: List[Tuple[datetime, datetime, Path]]) -> datetime:
    """If no files: from DEFAULT_END_DATE; else: oldest_start - 1 minute."""
    if not files:
        return datetime.strptime(DEFAULT_END_DATE, "%Y-%m-%d")
    oldest_start = files[0][0]
    return oldest_start - timedelta(minutes=1)

def pair_is_complete(pair_caret: str) -> bool:
    """True if the pair's earliest window already reaches STOP_BOUNDARY (or earlier)."""
    files, _ = scan_aligned_files(pair_caret, DOWNLOAD_DIR)
    if not files:
        return False
    oldest_start = files[0][0]
    return oldest_start <= STOP_BOUNDARY

# ---------- login + navigate ----------
def login_and_open_download():
    driver.get("https://www.barchart.com/login")
    for xp in ["//button[contains(.,'Accept')]", "//button[contains(.,'Agree')]", "//button[contains(.,'I Agree')]"]:
        maybe_click((By.XPATH, xp), 2)
    find((By.NAME, "email")).send_keys(EMAIL)
    find((By.NAME, "password")).send_keys(PASSWORD)
    for how, sel in [
        (By.XPATH, "//button[contains(.,'Login')]"),
        (By.CSS_SELECTOR, "button.bc-button-plain--blue"),
        (By.CSS_SELECTOR, "button[type='submit']"),
    ]:
        if maybe_click((how, sel), 6): break

    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, "header, .bc-logo, [data-role='user-menu']")))
    dismiss_overlays()

    # Tools → Historical Data Download
    tools_locator  = (By.CSS_SELECTOR, "a.show-for-desktop[href='/my/watchlist'], a.show_for_desktop[href='/my/watchlist']")
    target_locator = (By.CSS_SELECTOR, "a[href='/my/price-history/download']")
    for loc in (tools_locator, target_locator):
        try:
            click(loc, 10)
        except ElementClickInterceptedException:
            dismiss_overlays()
            el = WebDriverWait(driver, 10).until(EC.presence_of_element_located(loc))
            driver.execute_script("arguments[0].scrollIntoView({block:'center'});", el)
            time.sleep(0.1)
            try: el.click()
            except ElementClickInterceptedException:
                driver.execute_script("arguments[0].click();", el)
        time.sleep(0.2)

    ensure_in_download_context()
    WebDriverWait(driver, 10).until(lambda d: len(d.find_elements(By.CSS_SELECTOR, "a.download-btn, button.download-btn")) > 0)
    print("✓ On Historical Data Download page")

# ---------- core download (given a list of END dates) ----------
def do_work(symbol: str, end_dates: List[datetime], global_counter) -> int:
    """Returns number of successful downloads performed."""
    ensure_in_download_context()
    maybe_click((By.CSS_SELECTOR, "a.reset-btn"), 3)
    time.sleep(0.3)
    ensure_in_download_context()

    set_intraday_1min()
    fill_symbol(symbol)
    time.sleep(0.6)

    retries_left = MAX_RETRIES_PER_END
    done = 0
    idx = 0
    while idx < len(end_dates):
        if GLOBAL_MAX_DOWNLOADS is not None and global_counter["done"] >= GLOBAL_MAX_DOWNLOADS:
            print("Global download cap reached; stopping further work.")
            break

        end_cap = end_dates[idx]
        if end_cap < STOP_BOUNDARY:
            print("Reached stop boundary; stopping this pair.")
            break

        print(f"[{idx+1}/{len(end_dates)}] {symbol} → end {end_cap.date()} (align {DAYS_PER_CHUNK} CT days)")
        set_end_only(end_cap)
        time.sleep(0.4)

        btn = wait_api_config(symbol, expect_type="minutes", timeout=12)
        driver.execute_script("arguments[0].scrollIntoView({block:'center'});", btn)
        time.sleep(0.1)
        try:
            btn.click()
        except ElementClickInterceptedException:
            dismiss_overlays()
            driver.execute_script("arguments[0].click();", btn)

        p = wait_for_download(timeout=180)
        if not p:
            print("  ⚠ Download timeout; retrying")
            if retries_left > 0:
                retries_left -= 1
            else:
                print("  ✕ Out of retries for this end; skipping")
                retries_left = MAX_RETRIES_PER_END
                idx += 1
            continue

        # Trim to exact CT window and SNAP filename bounds
        try:
            trim_oldest, trim_newest, df_trim, expected_minutes = trim_to_fixed_window(p, end_cap)
            got = len(df_trim)
            if got < int(expected_minutes * MIN_COVERAGE_RATIO):
                raw_new_path = DOWNLOAD_DIR / (Path(p).stem + "_RAW.csv")
                os.replace(p, raw_new_path)
                print(f"  ⚠ Micro-chunk {got}/{expected_minutes} mins. Saved RAW: {raw_new_path.name}")
                if retries_left > 0:
                    retries_left -= 1
                    continue
                else:
                    print("  ✕ Out of retries; skipping")
                    retries_left = MAX_RETRIES_PER_END
                    idx += 1
                    continue
        except Exception as e:
            print("  ⚠ Could not trim/align; saving RAW and skipping:", e)
            raw_new_path = DOWNLOAD_DIR / (Path(p).stem + "_RAW.csv")
            os.replace(p, raw_new_path)
            retries_left = MAX_RETRIES_PER_END
            idx += 1
            continue

        new_name = "{}_{}-{}.csv".format(
            symbol.lstrip('^'),
            trim_oldest.strftime("%Y%m%d%H%M"),
            trim_newest.strftime("%Y%m%d%H%M"),
        )
        df_trim.to_csv(DOWNLOAD_DIR / new_name, index=False)
        os.remove(p)
        print(f"  ✓ Saved ({got}/{expected_minutes} mins): {new_name}")

        retries_left = MAX_RETRIES_PER_END
        idx += 1
        done += 1
        global_counter["done"] += 1

    return done

# ---------- planning per pair ----------
def build_work_for_pair(pair_caret: str, max_chunks: int) -> List[datetime]:
    """Returns a list of END dates to run (repairs first, else backfill), capped to max_chunks."""
    files, misaligned = scan_aligned_files(pair_caret, DOWNLOAD_DIR)

    if misaligned:
        print(f"[{pair_caret}] Deleting misaligned files (after DST-tolerant check):")
        for p in misaligned:
            print("  -", p.name)
            try: p.unlink()
            except Exception as e: print("    (couldn't delete)", e)

    # Re-scan after deletes
    files, _ = scan_aligned_files(pair_caret, DOWNLOAD_DIR)
    repairs, deletes = plan_repairs_and_deletes(files)

    if deletes:
        print(f"[{pair_caret}] Deleting overlapping neighbors:")
        for p in deletes:
            print("  -", p.name)
            try: p.unlink()
            except Exception as e: print("    (couldn't delete)", e)

    # Re-scan & recompute repairs one more time
    files, _ = scan_aligned_files(pair_caret, DOWNLOAD_DIR)
    repairs, _ = plan_repairs_and_deletes(files)

    work: List[datetime] = []
    if repairs:
        work = repairs[:max_chunks]
        print(f"[{pair_caret}] Planned repairs: {len(repairs)} (processing {len(work)} now)")
    else:
        # no repairs → extend backward
        start_end = find_backward_end(files)
        tm = timedelta(minutes=TARGET_WINDOW_MIN)
        cur = start_end
        while len(work) < max_chunks and cur >= STOP_BOUNDARY:
            work.append(cur)
            cur = cur - tm
        if not files:
            print(f"[{pair_caret}] ▶ Fresh run: starting at END_DATE {start_end.date()}")
        else:
            print(f"[{pair_caret}] ↩ Resume backfill from {start_end}")
    return work

# ---------- main ----------
def main():
    global_counter = {"done": 0}
    try:
        login_and_open_download()

        for pair_caret in [p.upper() for p in PAIRS]:  # already contains '^'; no extra caret added
            if GLOBAL_MAX_DOWNLOADS is not None and global_counter["done"] >= GLOBAL_MAX_DOWNLOADS:
                print("Global cap reached — stopping all pairs.")
                break

            print("\n" + "="*80)
            print(f"Pair: {pair_caret}")

            # Skip if already complete (earliest window reaches STOP_BOUNDARY)
            if pair_is_complete(pair_caret):
                print(f"[{pair_caret}] Already complete to {STOP_BOUNDARY.date()} — skipping.")
                continue

            # Build plan for this pair (max MAX_PER_PAIR)
            work = build_work_for_pair(pair_caret, MAX_PER_PAIR)
            if not work:
                print(f"[{pair_caret}] Nothing to do.")
                continue

            # Clamp against remaining global budget
            if GLOBAL_MAX_DOWNLOADS is not None:
                remaining_global = GLOBAL_MAX_DOWNLOADS - global_counter["done"]
                if remaining_global <= 0:
                    print("Global cap reached — stopping all pairs.")
                    break
                if len(work) > remaining_global:
                    work = work[:remaining_global]
                    print(f"[{pair_caret}] Truncated work to fit global cap: {len(work)} tasks")

            # Execute plan
            done = do_work(pair_caret, work, global_counter)
            print(f"[{pair_caret}] Completed {done} downloads this run.")

            if GLOBAL_MAX_DOWNLOADS is not None and global_counter["done"] >= GLOBAL_MAX_DOWNLOADS:
                print("Global cap reached — stopping all pairs.")
                break

    finally:
        driver.quit()
        print("\nDone. Folder:", DOWNLOAD_DIR.resolve())

if __name__ == "__main__":
    main()

