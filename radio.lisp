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

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
