regexpescape = function(s) {
    return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
};


var clojurefunctions=["map", "range", "fn"];

clojurefunctions =
    "(" + clojurefunctions.map(regexpescape).join("|") + ")";

var tawnysecondaryfunction = [
    "not", "only",
    "owl-only", "owl-or","owl-some", "some-only"];
tawnysecondaryfunction =
    "(" + tawnysecondaryfunction.map(regexpescape).join("|") + ")";

Prism.languages.tawny = {
    comment: /;.*/,
    string: /(?:\"[^\"\\\n]*(?:\\.[^\"\\\n]*)*\")/,
    number: /\b[+-]?(?:\#i)?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][+-]?\d+)?\b/,
    punctuation: /[\[\]\(\)]/,
    lispkeyword: /[:][-a-zA-Z]*\b/,
    clojurefunction: new RegExp(clojurefunctions),
    tawnysecondaryfunction: new RegExp(tawnysecondaryfunction),
    tawnymainfunction: /\b(defclass|owl-class|defoproperty|object-property)\b/,
    owlentity: /(\b[a-z]*[A-Z]+[a-zA-Z]*\b)/
};
