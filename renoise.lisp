;; You need to have CM-INCUDINE installed. Normal CM just won't do, I'm pretty sure.
;; Incudine is used for realtime and osc

(defpackage :renoise
  (:use :cl :cm)
  (:export :renoise))

(in-package :renoise)

;; Setup where you want to send osc messages to. Both address and port.
(defparameter *address* "127.0.0.1")
(defparameter *port* 3091)
;; Define *renoise-out* for sending OSC messages
(defparameter *oscout* (incudine.osc:open :direction :output :host *address* :port *port*))
(cm::osc-open-default :host *address* :port *port* :direction :output)

(defun renoise (message &optional arg)
  (cond ((or (eql message 1) (eql message :start) (eql message :play)) (sprout
									(new cm::osc :time 0
									  :types ""
									  :message ""
									  :path "/renoise/transport/start")))
	((or (eql message 0) (eql message :stop)) (sprout
						   (new cm::osc :time 0
						     :types ""
						     :message ""
						     :path "/renoise/transport/stop")))
	((or (eql message 2) (eql message :continue)) (sprout
						       (new cm::osc :time 0
							 :types ""
							 :message ""
							 :path "/renoise/transport/continue")))
	((eql message :loop) (sprout
			      (new cm::osc :time 0
				:types "i"
				:message arg
				:path "/renoise/transport/loop/pattern")))
	((or (eql message 6) (eql message :panic)) (sprout
						    (new cm::osc :time 0
						      :types ""
						      :message ""
						      :path "/renoise/transport/panic")))
	((or (eql message :schedule) (eql message :schedule-pattern)) (sprout
								       (new cm::osc :time 0
									 :types "i"
									 :message arg
									 :path "/renoise/song/sequence/schedule_set")))
	((or (eql message :trigger) (eql message :trigger-pattern)
	     (eql message :pattern)) (sprout
				      (new cm::osc :time 0
					:types "i"
					:message (+ 1 arg)
					:path "/renoise/song/sequence/trigger")))
	((eql message :bpm) (sprout
			     (new cm::osc :time 0
			       :types "i"
			       :message arg
			       :path "/renoise/song/bpm")))
	((eql message :lpb) (sprout
			     (new cm::osc :time 0
			       :types "i"
			       :message arg
			       :path "/renoise/song/lpb")))
	((or (eql message :octave) (eql message :oct)) (sprout
							(new cm::osc :time 0
							  :types "i"
							  :message arg
							  :path "/renoise/song/edit/octave")))
	((eql message :edit) (sprout
			      (new cm::osc :time 0
				:types "i"
				:message arg
				:path "/renoise/song/edit/mode")))
	((or (eql message :metro) (eql message :metronome)) (sprout
							     (new cm::osc :time 0
							       :types "i"
							       :message arg
							       :path "/renoise/song/record/metronome")))
	((or (eql message :pre-metro) (eql message :metronome-precount)
	     (eql message :precount-metronome) (eql message :metro-pre)
	     (eql message :pre-metronome)) (sprout
					    (new cm::osc :time 0
					      :types "i"
					      :message arg
					      :path "/renoise/song/record/metronome_precount")))
	((eql message :solo) (sprout
			      (new cm::osc :time 0
				:types ""
				:message ""
				;; Use -1 as arg to solo currently selected track
				:path (concatenate 'string
						   "/renoise/song/track/" (write-to-string arg) "/solo"))))
	((eql message :mute) (sprout
			      (new cm::osc :time 0
				:types ""
				:message ""
				;; Use -1 as arg to solo currently selected track
				:path (concatenate 'string
						   "/renoise/song/track/" (write-to-string arg) "/mute"))))
	((eql message :unmute) (sprout
				(new cm::osc :time 0
				  :types ""
				  :message ""
				  ;; Use -1 as arg to solo currently selected track
				  :path (concatenate 'string
						     "/renoise/song/track/" (write-to-string arg) "/unmute"))))))


;; Some Examples Below:
;; Change octave of renoise
;; (renoise :oct 3)
;;
;; Change the transport's bpm
;; (renoise :bpm 123)
;;
;; Start/stop renoise's transport
;; (renoise :start)
;; (renoise :stop)
;;
;; You can even use a function to alter renoise's parameters
;; (renoise :bpm (random 150))
;;
;; Turn off everything, hits renoise's panic button
;; (renoise :panic)
;; 
;; Turn on pattern 1
;; (renoise :trigger-pattern 1)
;;
;; Queue up pattern 1 as the next pattern to play after playing the current one
;; (renoise :schedule-pattern 9)



