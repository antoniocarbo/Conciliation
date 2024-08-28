clear
set more off

local user "mariagabrielamiranda"

global star "star(* 0.1 ** 0.05 *** 0.01)"
global stars "label nogaps fragment nonumbers noobs star(* 0.10 ** 0.05 *** 0.01) collabels(none) booktabs"
	
if "`user'" == "Marco" {
	global directory "/Users/marcomedina/ITAM Seira Research Dropbox/Marco Alejandro Medina/conciliation"
	cd "$directory"
	}

	if "`user'" == "Marco Desktop" {
	global directory "C:/Users/Guest/ITAM Seira Research Dropbox/Marco Alejandro Medina/conciliation"
	cd "$directory"
}

if "`user'" == "mariagabrielamiranda" {
	global directory "/Users/mariagabrielamiranda/Desktop/Conciliation/Pre-experiment"
	cd 
}

