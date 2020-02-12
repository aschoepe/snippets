/*
  (c) 2020 Alexander Schoepe, Bochum, DE
  BSD-3 License
  scanDT(string, format) default format "Y-m-d H:M:S"

  let ds = DateScanner();
  ds.scanDT("24.07.2019 12") -> string "2019-07-24 12:00:00"
  ds.scanDT("24.07.2019 12", "Y-m-d") -> string "2019-07-24"
  ds.scanDT("24.07.2019 12", "@") -> javascript dateObj "Wed Jul 24 2019 12:00:00 GMT+0200 (Mitteleuropäische Sommerzeit)"
*/

"use strict";

function DateScanner() {
  this.scanDT = scanDT;

  let defaults = { "y": "0", "m": 1, "d": 1, "H": 0, "M": 0, "S": 0 };
  let monthNames = {
    "jan": 1, "ene": 1, "gen": 1, "tam": 1,
    "feb": 2, "fev": 2, "fÈv": 2, "hel": 2,
    "mrz": 3, "mar": 3, "m‰r": 3, "maa": 3,
    "apr": 4, "avr": 4, "abr": 4, "huh": 4,
    "mai": 5, "may": 5, "mei": 5, "mag": 5, "maj": 5, "tou": 5,
    "jun": 6, "jui": 6, "giu": 6, "kes": 6,
    "jul": 7, "jui": 7, "lug": 7, "hei": 7,
    "aug": 8, "aou": 8, "ao˚": 8, "ago": 8, "elo": 8,
    "sep": 9, "set": 9, "syy": 9,
    "okt": 10, "oct": 10, "out": 10, "ott": 10, "lok": 10,
    "nov": 11, "mar": 11,
    "dez": 12, "dec": 12, "dÈc": 12, "dic": 12, "des": 12, "jou": 12
  };

  const reEU = /^(\d+)\.(\d+)\.(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?\s*([+-])?(\d+)?:?(\d+)?$/;
  const mapEU = { "d": 1, "m": 2, "y": 3, "H": 4, "M": 5, "S": 6, "F": 7, "x": 8, "a": 9, "b": 10 };
  const reISO = /^(\d+)-(\d+)-(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?\s*([+-])?(\d+)?:?(\d+)?$/;
  const mapISO = { "y": 1, "m": 2, "d": 3, "H": 4, "M": 5, "S": 6, "F": 7, "x": 8, "a": 9, "b": 10 };
  const reOra = /^(\d+)-(\w+)-(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?\s*([+-])?(\d+)?:?(\d+)?$/;
  const mapOra = { "d": 1, "ml": 2, "y": 3, "H": 4, "M": 5, "S": 6, "F": 7, "x": 8, "a": 9, "b": 10 };
  const reUS = /(\d+)\/(\d+)\/(\d+)T?\s*(\d+)?:?(\d+)?:?(\d+)?\.?(\d+)?\s*([+-])?(\d+)?:?(\d+)?/;
  const mapUS = { "m": 1, "d": 2, "y": 3, "H": 4, "M": 5, "S": 6, "F": 7, "x": 8, "a": 9, "b": 10 };

  function mapDate(dtA, map) {
    let dtO = new Object();

    for (var key in map)
      dtO[key] = dtA[map[key]] == undefined ? defaults[key] : dtA[map[key]];
    return dtO;
  }

  function formatDate(dtO, fmt) {
    let r = fmt.replace(/[yYmdHMS]/g, function (key, offset, all) {
      var val = parseInt(dtO[key]);
      return val < 10 ? "0" + val : val;
    });
    return r;
  }

  function scanDT(dt, fmt) {
    let dtA;
    let dtO;

    if (fmt == undefined)
      fmt = "Y-m-d H:M:S";

    if ((dtA = dt.match(reEU)) != null)
      dtO = mapDate(dtA, mapEU);
    else if ((dtA = dt.match(reISO)) != null)
      dtO = mapDate(dtA, mapISO);
    else if ((dtA = dt.match(reOra)) != null) {
      dtO = mapDate(dtA, mapOra);
      dtO["m"] = monthNames[dtO["ml"]] || (parseInt(dtO["ml"].replace(/^0+/, "")));
    } else if ((dtA = dt.match(reUS)) != null)
      dtO = mapDate(dtA, mapUS);
    else
      return null;

    let y = parseInt(dtO["y"]);
    if (y < 100) {
      if (y < 50) {
        y += 2000;
      } else {
        y += 1900;
      }
    }
    dtO["Y"] = y;
    y %= 100;
    dtO["y"] = y;

    if (fmt == "@")
      return new Date(formatDate(dtO, "Y-m-d H:M:S"));
    else
      return formatDate(dtO, fmt);
  }
}
