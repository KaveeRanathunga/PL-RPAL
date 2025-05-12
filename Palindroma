let rec getFirstDegit n = ( n ge 0 & n < 10) -> n | getFirstDegit (n/10)
and
rec getNumDegits n = ( n ge 0 & n < 10) -> 0 | (1+ (getNumDegits (n/10)))
in
let rec getLastDegit n = ( n ge 0 & n < 10) -> n | getLastDegit (n - ((getFirstDegit n) * 10 ** (getNumDegits n)))
in
let remain n = (n - ((getFirstDegit n) * 10 ** (getNumDegits n)))/10
in
let rec checkPalindrome n = (n < 10 & n ge 0) -> true |
			((getFirstDegit n) eq (getLastDegit n)) -> (checkPalindrome (remain n)) | false
in
Print ( checkPalindrome 14144141, checkPalindrome 141441)
			