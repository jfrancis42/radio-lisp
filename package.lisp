;;;; package.lisp

(defpackage #:radio
  (:use #:cl)
  (:export :watts-to-milliwatts
	   :milliwatts-to-watts
	   :watts-to-dbm
	   :dbm-to-watts
	   :dipole-in-ft
	   :dipole-in-m
	   :calc-db-gain
	   :calc-watts-with-db-gain
	   :vpeak-to-vrms
	   :vrms-to-vpeak
	   :vrms-to-watts
	   :watts-to-vrms
	   :vpeak-to-watts
	   :watts-to-vpeak
	   :vpkpk-to-watts
	   :watts-to-vpkpk
	   :return-loss-db-to-vswr
	   :vswr-to-return-loss-db
	   :return-loss-db-to-mismatch-loss-db
	   :vswr-to-mismatch-loss-db
	   :vswr-to-percentage-loss
	   :tdr-length-meters
	   :tdr-length-feet
	   :tdr-velocity-factor-given-meters
	   :tdr-velocity-factor-given-feet
	   :closest-standard-resistor
	   :closest-standard-capacitor
	   :f-to-uf
	   :f-to-nf
	   :f-to-pf
	   :uf-to-f
	   :uf-to-nf
	   :uf-to-pf
	   :nf-to-f
	   :nf-to-uf
	   :nf-to-pf
	   :pf-to-f
	   :pf-to-uf
	   :pf-to-nf
	   :arrl-low-pass-af-active-filter
	   :arrl-high-pass-af-active-filter
	   ))
