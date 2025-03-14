;;;; package.lisp

(defpackage #:radio
  (:use #:cl)
  (:import-from #:jeffutils
		#:closest)
  (:export :watts-to-milliwatts
	   :milliwatts-to-watts
	   :watts-to-dbm
	   :dbm-to-watts
	   :dipole-in-ft
	   :dipole-in-m
	   :calc-db-gain
	   :calc-watts-with-db-gain
	   :vpeak-to-vrms
	   :vpeak-to-vpkpk
	   :vpkpk-to-vpeak
	   :vrms-to-vpeak
	   :vrms-to-watts
	   :watts-to-vrms
	   :vpeak-to-watts
	   :watts-to-vpeak
	   :vpkpk-to-watts
	   :vpkpk-to-vrms
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
	   :x-to-kilo
	   :x-to-mega
	   :kilo-to-mega
	   :mega-to-kilo
	   :kilo-to-x
	   :mega-to-x
	   :x-to-milli
	   :x-to-micro
	   :x-to-nano
	   :x-to-pico
	   :milli-to-x
	   :milli-to-nano
	   :milli-to-pico
	   :micro-to-x
	   :micro-to-milli
	   :micro-to-nano
	   :micro-to-pico
	   :nano-to-x
	   :nano-to-milli
	   :nano-to-micro
	   :nano-to-pico
	   :pico-to-x
	   :pico-to-milli
	   :pico-to-micro
	   :pico-to-nano
	   :arrl-low-pass-af-active-filter
	   :arrl-high-pass-af-active-filter
	   :*standard-resistors*
	   :*standard-capacitors*
	   :*standard-inductors*
	   :inductor-air-core-turns
	   :inductor-air-core-henries
	   :inductor-powdered-iron-core-turns
	   :inductor-powdered-iron-core-henries
	   :inductor-ferrite-core-turns
	   :inductor-ferrite-core-henries
	   :lc-inductance
	   :lc-capacitance
	   :lc-freq
	   :rad-sec-to-hz
	   :hz-to-rad-sec
	   :cm-to-in
	   :mm-to-in
	   :mm-to-cm
	   :cm-to-mm
	   :in-to-cm
	   :in-to-mm
	   :wire-awg-to-in
	   :wire-awg-to-mm
	   ))
