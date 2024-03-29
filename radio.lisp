;;;; radio.lisp

(in-package #:radio)

;;; Just for giggles, let's join the 21st Century.
(setf *read-default-float-format* 'double-float)

(defparameter *c* 299792458)

(defun watts-to-milliwatts (watts)
  "Given watts, returns the equivalent number of milliwatts."
  (* 1000 watts))

(defun milliwatts-to-watts (milliwatts)
  "Given milliwatts, returns the equivalent number of watts."
  (/ milliwatts 1000))

(defun watts-to-dbm (watts)
  "Convert watts to dBm."
  (* 10 (log (* watts 1000) 10)))

(defun dbm-to-watts (dbm)
  "Convert dBm to watts."
  (/ (expt 10 (/ dbm 10)) 1000))

(defun dipole-in-ft (fmhz)
  "Given a frequency in MHz, returns the length of a dipole in feet."
  (/ 468 fmhz))

(defun dipole-in-m (fmhz)
  "Given a frequency in MHz, returns the length of a dipole in meters."
  (/ 143 fmhz))

(defun calc-db-gain (watts-in watts-out)
  "Given an input power in watts and an output power in watts, returns the gain (-loss) in dB."
  (* 10 (log (/ watts-out watts-in) 10)))

(defun calc-watts-with-db-gain (watts-in db)
  "Given a power in watts and a gain (-loss) in dB, returns the value."
  (* watts-in (expt 10 (/ db 10))))

(defun vpeak-to-vpkpk (v-peak)
  (* v-peak 2))

(defun vpkpk-to-vpeak (v-pkpk)
  (/ v-pkpk 2))

(defun vpeak-to-vrms (v-peak)
  "Convert peak voltage to RMS voltage."
  (* (/ 1 (sqrt 2)) v-peak))

(defun vrms-to-vpeak (v-rms)
  "Given an RMS voltage, return the peak voltage."
  (* (sqrt 2) v-rms))

(defun vrms-to-watts (v-rms)
  "Given a VRMS voltage as a sine wave into a 50 ohm load, return the equivalent watts."
  (/ (* v-rms v-rms) 50))

(defun watts-to-vrms (watts)
  "Given a number of watts as a sine wave into a 50 ohm load, return the RMS voltage."
  (* 5 (sqrt 2) (sqrt watts)))

(defun vpeak-to-watts (v-peak)
  "Given a peak voltage as a sine wave into a 50 ohm load, return the number of watts."
  (/ (* v-peak v-peak) 100))

(defun watts-to-vpeak (watts)
  "Given watts as a sine wave into a 50 ohm load, return the peak voltage."
  (* 10 (sqrt watts)))

(defun vpkpk-to-watts (v-pkpk)
  "Given a peak-to-peak voltage as a sine wave into a 50 ohm load, return the number of watts."
  (/ (* v-pkpk v-pkpk) 400))

(defun vpkpk-to-vrms (v-pkpk)
  "Given a peak-to-peak voltage as a sine wave into a 50 ohm load, return the RMS voltage."
  (vpeak-to-vrms (vpkpk-to-vpeak v-pkpk)))

(defun watts-to-vpkpk (watts)
  "Given watts as a sine wave into a 50 ohm load, return the peak-to-peak voltage."
  (* 20 (sqrt watts)))

(defun return-loss-db-to-vswr (return-loss-db)
  "Given a return loss in dB, return the VSWR."
  (/ (+ 1 (expt 10 (/ (- 0 return-loss-db) 20)))
     (- 1 (expt 10 (/ (- 0 return-loss-db) 20)))))

(defun vswr-to-return-loss-db (vswr)
  "Given a VSWR, return the return loss in dB."
  (* 20 (log (/ (+ vswr 1) (- vswr 1)) 10)))

(defun return-loss-db-to-mismatch-loss-db (return-loss-db)
  "Given a return loss in dB, return the mismatch loss in dB."
  (* -10 (log (- 1 (expt (expt 10 (/ (- 0 return-loss-db) 20)) 2)) 10)))

(defun vswr-to-mismatch-loss-db (vswr)
  "Given a VSWR, return the mismatch loss in dB."
  (return-loss-db-to-mismatch-loss-db
   (vswr-to-return-loss-db vswr)))

(defun vswr-to-percentage-loss (vswr)
  "Given a VSWR, return the loss in percentage."
  (if (<= vswr 1.0)
      0
      (- 100
	 (* 100
	    (calc-watts-with-db-gain 1.0 (- 0 (vswr-to-mismatch-loss-db vswr)))))))

(defun tdr-length-meters (nanoseconds-round-trip &optional (velocity-factor (/ 2 3)))
  "Given the elapsed round-trip time in nanoseconds and a coax velocity factor (defaults to 0.67), return the coax length in meters."
  (* (/ *c* 1000000000) nanoseconds-round-trip 0.5 velocity-factor))

(defun tdr-length-feet (nanoseconds-round-trip &optional (velocity-factor (/ 2 3)))
  "Given the elapsed round-trip time in nanoseconds and a coax velocity factor (defaults to 0.67), return the coax length in feet."
  (* 3.281 (tdr-length-meters nanoseconds-round-trip velocity-factor)))

(defun tdr-velocity-factor-given-meters (nanoseconds-round-trip meters-length)
  "Given the elapsed round-trip time in nanonseconds and the length of the coax in meters, return the velocity factor."
  (when (> nanoseconds-round-trip 0)
    (* (/ (* 2 meters-length) nanoseconds-round-trip *c*) 1000000000)))

(defun tdr-velocity-factor-given-feet (nanoseconds-round-trip feet-length)
  "Given the elapsed round-trip time in nanonseconds and the length of the coax in feet, return the velocity factor."
  (when (> nanoseconds-round-trip 0)
    (tdr-velocity-factor-given-meters nanoseconds-round-trip (/ feet-length 3.281))))

(defun closest-standard-resistor (ohms)
  "Return the value of the closest standard resistor in ohms."
  (first
   (sort (list 0 1 2.2 3.3 4.7 10 22 47 68 100 150 220 330 470
	       680 1000 1500 2200 3300 4700 6800 10000 15000
	       22000 33000 47000 68000 100000 220000 470000 1000000)
	 (lambda (a b) (< (abs (- a ohms)) (abs (- b ohms)))))))

(defun closest-standard-capacitor (uf)
  "Return the value of the closest standard capacitor in uf."
  (first
   (sort (list 0.000010 0.0000020 0.000047 0.000100 0.000150
	       0.000220 0.000330 0.000470 0.000680 0.001 0.0022
	       0.0033 0.0047 0.0068 0.010 0.022 0.033 0.047
	       0.068 0.100 0.220 0.470 1 2.2 3.3 4.7 10 22 33
	       47 100 220 330 470 680 1000)
	 (lambda (a b) (< (abs (- a uf)) (abs (- b uf)))))))

(defun f-to-uf (c)
  "Convert from farads to microfarads."
  (* c 1000000))

(defun f-to-nf (c)
  "Convert from farads to nanofarads."
  (* c 1000000000))

(defun f-to-pf (c)
  "Convert from farads to picofarads."
  (* c 1000000000000))

(defun uf-to-f (c)
  "Convert from microfarads to farads."
  (/ c 1000000))

(defun uf-to-nf (c)
  "Convert from microfarads to nanofarads."
  (f-to-nf (uf-to-f c)))

(defun uf-to-pf (c)
  "Convert from microfarads to picofarads."
  (f-to-pf (uf-to-f c)))

(defun nf-to-f (c)
  "Convert from nanofarads to farads."
  (/ c 1000000000))

(defun nf-to-uf (c)
  "Convert from nanofarads to microfarads."
  (f-to-uf (nf-to-f c)))

(defun nf-to-pf (c)
  "Convert from nanofarads to picofarads."
  (f-to-pf (nf-to-f c)))

(defun pf-to-f (c)
  "Convert from picofarads to farads."
  (/ c 1000000000000))

(defun pf-to-uf (c)
  "Convert from picofarads to microfarads."
  (f-to-uf (pf-to-f c)))

(defun pf-to-nf (c)
  "Convert from picofarads to nanofarads."
  (f-to-nf (pf-to-f c)))

(defun stage (which of)
  "From the ARRL Handbook. Factor 'a' for low- and high-pass filters."
  (nth (- which 1)
       (nth (- of 1)
	    (list (list 1.414 nil nil nil)
		  (list 0.765 1.848 nil nil)
		  (list 0.518 1.414 1.932 nil)
		  (list 0.390 1.111 1.663 1.962)))))

(defun arrl-low-pass-af-active-filter (this-stage total-stages
				       gain minus-3db-cutoff-freq
				       &optional (closest-real t))
  "Calculate the component values for the simple Butterworth AF low-pass
active filter in the ARRL Handbook. If gain<=1, omit R3 and bypass
R4. Defaults to returning commercially available resistor/capactor
values. Figure 10.32 in the 2021 Handbook, similar in other editions."
  (let* ((k gain)
	 (fc minus-3db-cutoff-freq)
	 (wc (* 2 pi fc))
	 (a (stage this-stage total-stages))
	 (b 1)
	 (c2 (uf-to-f (/ 10.0 fc)))
	 (c1 (/ (* c2 (+ (expt a 2) (* 4 b (- k 1)))) (* 4 b)))
	 (r1 (/ 2 (* wc (+ (* a c2)
			   (sqrt (- (* c2 c2 (+ (* a a) (* 4 (- k 1))))
				    (* 4 c1 c2)))))))
	 (r2 (/ 1 (* b c1 c2 r1 (expt wc 2))))
	 (r3 (when (> k 1)
	       (/ (* k (+ r1 r2)) (- k 1))))
	 (r4 (if (> k 1)
		 (* k (+ r1 r2))
		 0)))
    (if closest-real
	(list (cons :c1 (closest-standard-capacitor (f-to-uf c1)))
	      (cons :c2 (closest-standard-capacitor (f-to-uf c2)))
	      (cons :r1 (closest-standard-resistor r1))
	      (cons :r2 (closest-standard-resistor r2))
	      (cons :r3 (when (> k 1) (closest-standard-resistor r3)))
	      (cons :r4 (closest-standard-resistor r4)))
	(list (cons :c1 (f-to-uf c1))
	      (cons :c2 (f-to-uf c2))
	      (cons :r1 r1)
	      (cons :r2 r2)
	      (cons :r3 (when (> k 1) r3))
	      (cons :r4 r4)))))

(defun arrl-high-pass-af-active-filter (this-stage total-stages
					gain minus-3db-cutoff-freq
				       &optional (closest-real t))
  "Calculate the component values for the simple Butterworth AF high-pass
active filter in the ARRL handbook. If gain<=1, omit R3 and bypass
R4. Defaults to returning commercially available resistor/capactor
values. Figure 10.32 in the 2021 Handbook, similar in other editions."
  (let* ((k gain)
	 (fc minus-3db-cutoff-freq)
	 (wc (* 2 pi fc))
	 (a (stage this-stage total-stages))
	 (c (uf-to-f (/ 10.0 fc)))
	 (r1 (/ 4 (* (+ a (sqrt (+ (* a a) (* 8 (- k 1))))) wc c)))
	 (r2 (/ 1 (* wc wc c c r1)))
	 (r3 (when (> k 1)
	       (/ (* k r1) (- k 1))))
	 (r4 (if (> k 1)
		 (* k r1)
		 0)))
    (if closest-real
	(list (cons :c (closest-standard-capacitor (f-to-uf c)))
	      (cons :r1 (closest-standard-resistor r1))
	      (cons :r2 (closest-standard-resistor r2))
	      (cons :r3 (when (> k 1) (closest-standard-resistor r3)))
	      (cons :r4 (closest-standard-resistor r4)))
	(list (cons :c (f-to-uf c))
	      (cons :r1 r1)
	      (cons :r2 r2)
	      (cons :r3 (when (> k 1) r3))
	      (cons :r4 r4)))))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
