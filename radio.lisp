 ;;;; radio.lisp

(in-package #:radio)

;;; Just for giggles, let's join the 21st Century.
(setf *read-default-float-format* 'double-float)

(defparameter *toroids* (make-hash-table :test #'equal))
(defparameter *c* 299792458)

(defun cm-to-in (cm)
  "Convert cm to inches."
  (/ cm 2.54))

(defun mm-to-in (mm)
  "Convert mm to inches."
  (/ (cm-to-in mm) 10))

(defun mm-to-cm (mm)
  "Convert mm to cm."
  (/ mm 10))

(defun cm-to-mm (mm)
  "Convert cm to mm."
  (* mm 10))

(defun in-to-cm (in)
  "Convert in to cm."
  (* 2.54 in))

(defun in-to-mm (in)
  "Convert in to mm."
  (* 10 (in-to-cm in)))

(defun wire-awg-to-in (awg)
  "Calculate wire diameter in inches from AWG."
  (* 0.005 (expt 92 (/ (- 36 awg) 39))))

(defun wire-awg-to-mm (awg)
  "Calculate wire diameter in mm from AWG."
  (in-to-mm (wire-awg-to-in awg)))

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

(defun dipole-in-ft (fmhz &optional (velocity-factor 1.0))
  "Given a frequency in MHz, returns the length of a dipole in feet."
  (/ (/ 468 fmhz) velocity-factor))

(defun dipole-in-m (fmhz &optional (velocity-factor 1.0))
  "Given a frequency in MHz, returns the length of a dipole in meters."
  (/ (/ 143 fmhz) velocity-factor))

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

(defparameter *standard-resistors*
  (list 0 1 2.2 3.3 4.7 10 22 47 68 100 150 220 330 470
	680 1000 1500 2200 3300 4700 6800 10000 15000
	22000 33000 47000 68000 100000 220000 470000 1000000))

(defparameter *standard-capacitors*
  (list 1.0e-11 2.0e-11 4.7e-11 1.0e-10 1.5e-10 2.2e-10 3.3e-10 4.7e-10
	6.8e-10 1.0e-9 2.2e-9 3.3e-9 4.7e-9 6.8e-9 1.0e-8 2.2e-8 3.3e-8
	4.7e-8 6.8e-8 1.0e-7 2.2e-7 4.7e-7 1.0e-6 2.2e-6 3.3e-6 4.7e-6
	1.0e-5 2.2e-5 3.3e-5 4.7e-5 1.0e-4 2.2e-4 3.3e-4 4.7e-4 6.8e-4
	0.001))

(defparameter *standard-inductors*
  (list 1.0e-6 2.2e-6 4.7e-6 6.8e-6 1.0e-5 1.5e-5 2.2e-5
	3.3e-5 4.7e-5 6.8e-5 1.0e-4 1.5e-4 2.2e-4 3.3e-4
	4.7e-4 6.8e-4 0.001 0.0022 0.0047 0.01))

(defun closest-standard-resistor (ohms)
  "Return the value of the closest standard resistor in ohms."
  (first (closest *standard-resistors* ohms)))

(defun closest-standard-capacitor (f)
  "Return the value of the closest standard capacitor in f."
  (first (closest *standard-capacitors* f)))

(defun closest-standard-inductor (h)
  "Return the value of the closest standard inductor in h."
  (first (closest *standard-inductors* h)))

(defun x-to-kilo (x)
  "Convert from things to kilothings."
  (/ x 1000))

(defun x-to-mega (x)
  "Convert from things to megathings."
  (/ x 1000000))

(defun kilo-to-mega (x)
  "Convert from kilothings to megathings."
  (/ x 1000))

(defun mega-to-kilo (x)
  "Convert from megathings to kilothings."
  (* x 1000))

(defun kilo-to-x (x)
  "Convert from kilothings to things."
  (* x 1000))

(defun mega-to-x (x)
  "Convert from megathings to things."
  (* x 1000000))

(defun x-to-milli (x)
  "Convert from things to millithings."
  (* x 1000))

(defun x-to-micro (x)
  "Convert from things to microthings."
  (* x 1000000))

(defun x-to-nano (x)
  "Convert from things to nanothings."
  (* x 1000000000))

(defun x-to-pico (x)
  "Convert from things to picothings."
  (* x 1000000000000))

(defun milli-to-x (x)
  "Convert from millithings to things."
  (/ x 1000))

(defun milli-to-nano (x)
  "Convert from millithings to nanothings."
  (x-to-nano (milli-to-x x)))

(defun milli-to-pico (x)
  "Convert from millithings to picothings."
  (x-to-pico (milli-to-x x)))

(defun micro-to-x (x)
  "Convert from microthings to things."
  (/ x 1000000))

(defun micro-to-milli (x)
  "Convert from microthings to millithings."
  (x-to-milli (micro-to-x x)))

(defun micro-to-nano (x)
  "Convert from microthings to nanothings."
  (x-to-nano (micro-to-x x)))

(defun micro-to-pico (x)
  "Convert from microthings to picothings."
  (x-to-pico (micro-to-x x)))

(defun nano-to-x (x)
  "Convert from nanothings to things."
  (/ x 1000000000))

(defun nano-to-milli (x)
  "Convert from nanothings to millithings."
  (x-to-milli (nano-to-x x)))

(defun nano-to-micro (x)
  "Convert from nanothings to microthings."
  (x-to-micro (nano-to-x x)))

(defun nano-to-pico (x)
  "Convert from nanothings to picothings."
  (x-to-pico (nano-to-x x)))

(defun pico-to-x (x)
  "Convert from picothings to things."
  (/ x 1000000000000))

(defun pico-to-milli (x)
  "Convert from picothings to millithings."
  (x-to-milli (pico-to-x x)))

(defun pico-to-micro (x)
  "Convert from picothings to microthings."
  (x-to-micro (pico-to-x x)))

(defun pico-to-nano (x)
  "Convert from picothings to nanothings."
  (x-to-nano (pico-to-x x)))

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
	 (c2 (micro-to-x (/ 10.0 fc)))
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
	(list (cons :c1 (closest-standard-capacitor (x-to-micro c1)))
	      (cons :c2 (closest-standard-capacitor (x-to-micro c2)))
	      (cons :r1 (closest-standard-resistor r1))
	      (cons :r2 (closest-standard-resistor r2))
	      (cons :r3 (when (> k 1) (closest-standard-resistor r3)))
	      (cons :r4 (closest-standard-resistor r4)))
	(list (cons :c1 (x-to-micro c1))
	      (cons :c2 (x-to-micro c2))
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
	 (c (micro-to-x (/ 10.0 fc)))
	 (r1 (/ 4 (* (+ a (sqrt (+ (* a a) (* 8 (- k 1))))) wc c)))
	 (r2 (/ 1 (* wc wc c c r1)))
	 (r3 (when (> k 1)
	       (/ (* k r1) (- k 1))))
	 (r4 (if (> k 1)
		 (* k r1)
		 0)))
    (if closest-real
	(list (cons :c (closest-standard-capacitor (x-to-micro c)))
	      (cons :r1 (closest-standard-resistor r1))
	      (cons :r2 (closest-standard-resistor r2))
	      (cons :r3 (when (> k 1) (closest-standard-resistor r3)))
	      (cons :r4 (closest-standard-resistor r4)))
	(list (cons :c (x-to-micro c))
	      (cons :r1 r1)
	      (cons :r2 r2)
	      (cons :r3 (when (> k 1) r3))
	      (cons :r4 r4)))))

(defun parallel (v)
  "Calculate the resistance of a list of resistors in parallel."
  (/ 1.0 (apply #'+ (mapcar (lambda (a) (/ 1 a)) (remove nil v)))))

(defun series (v)
  "Calculate the resistance of a list of resistors in series."
  (apply #'+ (remove nil v)))

(defun fart (xs k)
  (let ((x (car xs)))
    (cond
     ((null xs) nil)
     ((= k 1) (mapcar #'list xs))
     (t (append (mapcar (lambda (ys) (cons x ys))
			(fart xs (1- k)))
		(fart (cdr xs) k))))))

(defun fix-combos (c)
  (rest
   (remove-duplicates
    (mapcar
     (lambda (n)
       (sort n
	     (lambda (a b)
	       (cond
		 ((equal nil a) t)
		 ((equal nil b) t)
		 (t (< a b))))))
     (fart (cons nil (remove 0 c)) 3))
    :test #'equal)))

(defun combine (c)
  (assert (and (>= (length c) 1)
	       (<= (length c) 3)))
  (cond
    ((equal 1 (length c))
     (cons :just (list c (list c))))
    ((equal (length c) 2)
     (list
      (cons :series (list c (series c)))
      (cons :parallel (list c (parallel c)))))
    (t
     (list
      (cons :series (list c (series c)))
      (cons :parallel (list c (parallel c)))
      (cons :parallel-series-series
	    (list c
		  (parallel
		   (list
		    (series (list (first c) (second c)))
		    (third c)))))
      (cons :parallel-series-series
	    (list (list (first c) (third c) (second c))
		  (parallel
		   (list
		    (series (list (first c) (third c)))
		    (second c)))))
      (cons :series-parallel-parallel
	    (list c
		  (series
		   (list
		    (parallel (list (first c) (second c)))
		    (third c)))))
      (cons :series-parallel-parallel
	    (list (list (first c) (third c) (second c))
		  (series
		   (list
		    (parallel (list (first c) (third c)))
		    (second c)))))))))

(defun closest-combo (stuff target)
  "Given a list of stuff, returns the value closest to the target (modified)."
  (sort stuff
	(lambda (a b) (< (abs (- (first (last a)) target)) (abs (- (first (last b)) target))))))

(defun closest-resistance (target-ohms &optional (number-of-results 1))
  "Find the closest resistor combo to the target value using up to three standard resistors."
  (let ((n nil))
    (mapcar
     (lambda (ls)
       (mapcar
	(lambda (i)
	  (setf n (cons i n))) ls))
     (mapcar #'combine
	     (fix-combos *standard-resistors*)))
    (subseq (closest-combo n target-ohms) 0 number-of-results)))

(defun closest-capacitance (target-farads &optional (number-of-results 1))
  "Find the closest capacitor combo to the target value using up to three standard capacitors. xxx serial/parallel thing"
  (let ((n nil))
    (mapcar
     (lambda (ls)
       (mapcar
	(lambda (i)
	  (setf n (cons i n))) ls))
     (mapcar #'combine
	     (fix-combos *standard-capacitors*)))
    (subseq (closest-combo n target-farads) 0 number-of-results)))

(defun closest-inductance (target-henries &optional (number-of-results 1))
  "Find the closest inductor combo to the target value using up to three standard inductors."
  (let ((n nil))
    (mapcar
     (lambda (ls)
       (mapcar
	(lambda (i)
	  (setf n (cons i n))) ls))
     (mapcar #'combine
	     (fix-combos *standard-inductors*)))
    (subseq (closest-combo n target-henries) 0 number-of-results)))

(defun reasonable-value-res (r)
  (and (>= r 1)
       (< r 10)))

(defun reasonable-value-cap (n)
  (and (> n 0.01)
       (< n 1000)))

(defun sane-res (r)
  (cond
    ((reasonable-value-res r)
     (cons :ohms r))
    ((reasonable-value-res (x-to-kilo r))
     (cons :k-ohms (x-to-kilo r)))
    ((reasonable-value-res (x-to-mega r))
     (cons :m-ohms (x-to-mega r)))
    ((>= r 10000000)
     (cons :m-ohms (x-to-mega r)))
    ((< r 1)
     (cons :ohms r))
    (t
     (cons :ohms r))))

(defun sane-ind (h)
  (cond
    ((reasonable-value-cap h)
     (cons :henry h))
    ((reasonable-value-cap (x-to-milli h))
     (cons :milli-henry (x-to-milli h)))
    ((reasonable-value-cap (x-to-micro h))
     (cons :micro-henry (x-to-micro h)))
    ((reasonable-value-cap (x-to-nano h))
     (cons :nano-henry (x-to-nano h)))
    ((reasonable-value-cap (x-to-pico h))
     (cons :pico-henry (x-to-pico h)))
    ((>= h 10)
     (cons :henry h))
    ((< (x-to-pico h) 1)
     (cons :pico-henry (x-to-pico h)))
    (t
     (cons :henry h))))

(defun sane-cap (c)
  (cond
    ((reasonable-value-cap c)
     (cons :farad c))
    ((reasonable-value-cap (x-to-milli c))
     (cons :milli-farad (x-to-milli c)))
    ((reasonable-value-cap (x-to-micro c))
     (cons :micro-farad (x-to-micro c)))
    ((reasonable-value-cap (x-to-nano c))
     (cons :nano-farad (x-to-nano c)))
    ((reasonable-value-cap (x-to-pico c))
     (cons :pico-farad (x-to-pico c)))
    ((>= c 10)
     (cons :farad c))
    ((< (x-to-pico c) 1)
     (cons :pico-farad (x-to-pico c)))
    (t
     (cons :farad c))))

(defun inductor-air-core-turns (henries diameter-in length-in)
  "Given the desired inductance of an air-core inductor in henries and a
form diameter and length in inches, return the number of turns
required."
  (let ((uh (x-to-micro henries)))
    (/ (sqrt (* uh (+ (* (* 18 diameter-in) (* 40 length-in)))))
       diameter-in)))

(defun inductor-air-core-henries (turns diameter-in length-in)
  "Given the turns and the form diameter and length in inches of an
air-core inductor, return the inductance in henries."
  (micro-to-x
   (/ (* diameter-in diameter-in turns turns)
      (+ (* 18 diameter-in) (* 40 length-in)))))

(defun inductor-powdered-iron-core-turns (henries al)
  "Given the desired inductance of a powdered iron core inductor in
henries and the AL value of the core, return the number of turns
required."
  (let ((uh (x-to-micro henries)))
    (* 100 (sqrt (/ uh al)))))

(defun inductor-powdered-iron-core-henries (turns al)
  "Given the turns and AL core value of a powdered-iron core inductor,
return the inductance in henries."
  (micro-to-x
   (/ (* al turns turns)
      10000)))

(defun inductor-ferrite-core-turns (henries al)
  "Given the desired inductance of a ferrite core inductor in
henries and the AL value of the core, return the number of turns
required."
  (let ((mh (x-to-milli henries)))
    (* 1000 (sqrt (/ mh al)))))

(defun inductor-ferrite-core-henries (turns al)
  "Given the turns and AL core value of a ferrite core inductor,
return the inductance in henries."
  (milli-to-x
   (/ (* al turns turns)
      1000000)))

(defun lc-inductance (c f)
  "Given c (farads) and frequency (hz), return inductance (henries)."
  (/ (expt (/ 1 (* 2 pi f)) 2) c))

(defun lc-capacitance (l f)
  "Given inductance (henries) and frequency (hz), return
capacitance (farads)."
  (/ 1 (* 4 pi pi f f l)))

(defun lc-freq (l c)
  "Given inductance (henries) and capacitance (farads), return
frequency (hz)."
  (/ 1 (* 2 pi (sqrt c) (sqrt l))))

(defun rad-sec-to-hz (rad-sec)
  "Convert radians per second to hertz."
  (/ rad-sec (* 2 pi)))

(defun hz-to-rad-sec (hz)
  "Convert hertz to radians per second."
  (* 2 pi hz))

(defun q (center-freq-hz bandwidth-hz)
  "Calculate Q."
  (/ (abs center-freq-hz) (abs bandwidth-hz)))

(defun load-amps (resistance-ohms volts-drop)
  "Given the voltage drop across a series resistor and the value
 of that resistor, compute amps drawn."
  (/ volts-drop resistance-ohms))

(defun load-watts (resistance-ohms volts-drop volts-source)
  "Given the voltage drop across a series resistor, the value
 of that resistor, and the supply voltage, compute watts consumed."
  (* (load-amps resistance-ohms volts-drop) volts-source))

(defclass toroid ()
  ((size :accessor size :initarg :size :initform nil)
   (al :accessor al :initarg :al :initform nil)
   (od-in :accessor od-in :initarg :od-in :initform nil)
   (id-in :accessor id-in :initarg :id-in :initform nil)
   (ht-in :accessor ht-in :initarg :ht-in :initform nil)
   (color :accessor color :initarg :color :initform nil)))

; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))
; 
; (setf (gethash :foo1 *toroids*)
;       (make-instance 'toroid
; 		     :color :foo2
; 		     :ht-in foo3
; 		     :id-in foo4
; 		     :od-in foo5
; 		     :al foo6
; 		     :size foo1))

;(defun radar-range (&key :p-ave
;			 :g-tx
;			 :a-rx
;			 :p-rx
;			 (:o 1)
;			 (:n 1)
;			 :e-i-n
;			 :l-s
;			 (:a 0)
;			 :f-n
;			 (:k 1.38e-7)
;			 (:t-o 290)
;			 :b-n
;			 :r
;			 :f-r
;			 (:snr-1 13.8))
;  (/ (* p-ave g-tx a-rx p-rx o n e-i-n (expt


;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
