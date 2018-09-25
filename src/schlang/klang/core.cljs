(ns schlang.klang.core
  (:require [cljs-bach.synthesis :as k]
            [leipzig.canon]
            [leipzig.melody :refer [all bpm is phrase tempo then times where with having]]
            [leipzig.chord]
            [leipzig.temperament :as temperament]
            [leipzig.scale :as scale]
            [schlang.klang.instruments :as instruments]
            [schlang.klang.music :as music]))

;(.-currentTime context)
;(leipzig.melody/duration melody)

(defonce context (k/audio-context))

(defn ping [freq]
  (k/connect->
    (k/square freq)         ; Try a sawtooth wave.
    (k/percussive 0.01 0.4) ; Try varying the attack and decay.
    (k/gain 0.1)))          ; Try a bigger gain.

; Play the ping synthesiser now, at 440 hertz.
(defn play-tone! [tone]
  (-> tone
     (k/connect-> k/destination)
     (k/run-with context (k/current-time context) 1.0)))

(defn synth [note]
  (k/connect->
    (k/add (k/square (* 1.01 (:pitch note)))
           (k/sawtooth (:pitch note)))
    (k/low-pass 600)
    (k/adsr 0.001 0.4 0.5 0.1)
    (k/gain 0.15)))


(def melody
  ; The durations and pitches of the notes. Try changing them.
  (->> (phrase [1 0.5 0.5 1 1 2 2]
               [0 1 0 2 -3 1 -1])
       (all :instrument synth))) ; Here we choose the instrument.

(def harmony
  ; The durations and pitches of the harmony's notes.
  (->> (phrase [1 0.5 0.5 1 1 0.5 0.5 0.5 0.5 2]
               [4 4 5 4 7 6 7 6 5 4])
       (all :instrument synth))) ; bell, wah, organ and marimba are also instruments.

(def bass
  ; The durations and pitches of the bass.
  (->> (phrase (cycle [3 0.75 0.25])
               [-14 -15 -16 -17 -16 -15])
       (all :instrument instruments/wah)))

(def variation
  ; The durations and pitches of the bass.
  (->> (phrase [3 1 1.5 2.5]
               [9 11 8 10])
       (all :instrument instruments/marimba)))

(def beat
  (->> (phrase [1 0.5 0.5 1 1]
               (repeat 0))
       (having :instrument [instruments/kick instruments/closed-hat instruments/kick instruments/kick instruments/closed-hat])
       (times 2)))

(def m (->> melody
            ;(with harmony)
            ;(with bass)
            ;(with variation)
            ;(with beat)
           (tempo (bpm 90))
           (where :pitch (comp scale/C scale/major))))

(play-tone! (ping 440))
(music/play! context m)