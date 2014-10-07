Prism.languages.tawny = {
	comment: /;.*/,
	string: /(?:\"[^\"\\\n]*(?:\\.[^\"\\\n]*)*\")/,
	number: /\b[+-]?(?:\#i)?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][+-]?\d+)?\b/,
	keyword: /\b(?:define|lambda|cond|else|let\*|let)\b/,
	punctuation: /[\[\]\(\)]/,
        lispkeyword: /[:]\w*\b/,
        tawnyfunction: /\b(defclass|owl-class)\b/
}
