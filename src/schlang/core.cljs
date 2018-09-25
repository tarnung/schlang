(ns schlang.core
  (:require [play-cljs.core :as p]
            [schlang.klang.core :as k]
            [schlang.vector :as v]))

(enable-console-print!)

(declare menu-screen main-screen)

(def initial-state (let [x 12
                         y 12
                         x2 (int (/ x 2))]
                     {:width  500
                      :height 500
                      :speed  400
                      :game   {:width  x
                               :height y
                               :score  0
                               :food   [[x2 1]]}
                      :snake  {:body  [[x2 (-> y dec dec dec)]
                                       [x2 (-> y dec dec)]
                                       [x2 (-> y dec)]]
                               :eaten false}
                      :player {:key-pressed :none
                               :direction   :up}}))

(defonce *state (atom initial-state))

(defonce game (let [width (:width @*state)
                    height (:height @*state)]
                (p/create-game width height)))

(defn next-direction [direction key-pressed]
  (let [ctrl {:right {:up    :right
                      :right :down
                      :down  :left
                      :left  :up}
              :left  {:up    :left
                      :left  :down
                      :down  :right
                      :right :up}}]
    (if (#{:up :right :down :left} key-pressed)
      (-> ctrl key-pressed direction)
      direction)))

(defn next-position [[x y] direction]
  (let [state @*state
        width (-> state :game :width)
        height (-> state :game :height)
        assumed-position (case direction
                           :up [x (dec y)]
                           :down [x (inc y)]
                           :left [(dec x) y]
                           :right [(inc x) y])
        position-checked-x (let [[new-x new-y] assumed-position]
                             (cond (>= new-x width) [0 new-y]
                                   (< new-x 0) [(dec width) new-y]
                                   :default assumed-position))
        position-checked-y (let [[new-x new-y] position-checked-x]
                             (cond (>= new-y height) [new-x 0]
                                   (< new-y 0) [new-x (dec height)]
                                   :default position-checked-x))]
    position-checked-y))

#_(events/listen js/window "mousedown" (fn []))

(defn new-food [s]
  (let [width (-> s :game :width)
        height (-> s :game :height)
        points (repeatedly (fn [] [(rand-int width) (rand-int height)]))
        occupied (reduce into #{} [(-> s :game :food) (-> s :snake :body)])]
    (some #(if (not (occupied %))
             %)
          points)))

(defn check-food [s]
  (let [food (into #{} (-> s :game :food))
        body (-> s :snake :body)
        head (first body)
        eat (food head)]
    (if eat
      (-> s
          (update-in [:game :food] (fn [food] (remove #(= % head) food)))
          (update-in [:game :food] #(conj % (new-food s)))
          (assoc-in [:snake :eaten] true))
      s)))

(defn step []
  (let [state @*state
        old-direction (-> state :player :direction)
        key-pressed (-> state :player :key-pressed)
        new-direction (next-direction old-direction key-pressed)
        old-body (-> state :snake :body)
        old-head (first old-body)
        new-head (next-position old-head new-direction)
        eaten (-> state :snake :eaten)
        new-body (into [new-head] (if eaten
                                    old-body
                                    (drop-last old-body)))
        lost (not (apply distinct? new-body))]
    (if lost
      (p/set-screen game menu-screen)
      (swap! *state
             (fn [state]
               (if lost
                 initial-state
                 (-> state
                     (assoc-in [:snake :body] new-body)
                     (assoc-in [:player :direction] new-direction)
                     (assoc-in [:snake :eaten] false)
                     (assoc-in [:player :key-pressed] :move-executed)
                     check-food)))))))

; define a screen, where all the action takes place
(def main-screen
  (reify p/Screen

    ; runs when the screen is first shown
    (on-show [this]
      (reset! *state initial-state)
      (swap! *state (fn [s]
                      (-> s
                          (update-in [:timeoutid]
                                     (fn [_]
                                       (js/setInterval
                                         step
                                         (:speed s))))))))

    ; runs when the screen is hidden
    (on-hide [this]
      (js/clearInterval (:timeoutid @*state)))

    ; runs every time a frame must be drawn (about 60 times per sec)
    (on-render [this]
      (p/render game
                (let [state @*state
                      width (:width state)
                      height (:height state)
                      game-width (-> state :game :width)
                      game-height (-> state :game :height)
                      tile-width (/ width game-width)
                      tile-height (/ height game-height)
                      body (-> state :snake :body)
                      part-of-body (into #{} body)
                      food (into #{} (-> state :game :food))]
                  [[:stroke {:color "gray"}
                    (for [x (range game-width) y (range game-height)]
                      [:fill {:color (cond (= [x y] (first body)) "darkgray"
                                           (part-of-body [x y]) "gray"
                                           (food [x y]) "lightgreen"
                                           :default "lightblue")}
                       [:rect {:x      (* x tile-width)
                               :y      (* y tile-height)
                               :width  tile-width
                               :height tile-height}
                        (cond (= [x y] (first body))
                              (let [old-direction (-> state :player :direction)
                                    key-pressed (-> state :player :key-pressed)
                                    new-direction (next-direction old-direction key-pressed)
                                    eye-width (/ tile-width 4)
                                    eye-height (/ tile-width 4)]
                                [:fill {:color "red"}
                                 [:rect {:x      (case new-direction
                                                   :left 0
                                                   :right (- tile-width eye-width)
                                                   (- (/ tile-width 4) (/ eye-width 2)))
                                         :y      (case new-direction
                                                   :up 0
                                                   :down (- tile-height eye-height)
                                                   (- (/ tile-height 4) (/ eye-height 2)))
                                         :height eye-height
                                         :width  eye-width}]
                                 [:rect {:x      (case new-direction
                                                   :left 0
                                                   :right (- tile-width eye-width)
                                                   (- (* 3 (/ tile-width 4)) (/ eye-width 2)))
                                         :y      (case new-direction
                                                   :up 0
                                                   :down (- tile-height eye-height)
                                                   (- (* 3 (/ tile-height 4)) (/ eye-height 2)))
                                         :height eye-height
                                         :width  eye-width}]])
                              (food [x y])
                              (let [eye-width (/ tile-width 4)
                                    eye-height (/ tile-width 4)
                                    pupil-width (/ eye-width 2)
                                    pupil-height (/ eye-height 2)
                                    [x-offset y-offset] (v/unit-circle [x y] (first body))]
                                [:fill {:color "green"}
                                 [:rect {:x      (+ (- (/ tile-width 4) (/ eye-width 2))
                                                    (* (rand) (/ eye-width 4)))
                                         :y      (+ (- (/ tile-height 4) (/ eye-height 2))
                                                    (* (rand) (/ eye-width 4)))
                                         :height eye-height
                                         :width  eye-width}
                                  [:fill {:color "black"}
                                   [:rect {:x      (+ (- (/ eye-width 2)
                                                         (/ pupil-width 2))
                                                      (* (/ pupil-width 2) x-offset))
                                           :y      (+ (- (/ eye-height 2)
                                                         (/ pupil-height 2))
                                                      (* (/ pupil-height 2) y-offset))
                                           :height pupil-height
                                           :width  pupil-width}]]]
                                 [:rect {:x      (+ (- (* 3 (/ tile-width 4)) (/ eye-width 2))
                                                    (* (rand) (/ eye-width 4)))
                                         :y      (+ (- (/ tile-height 4) (/ eye-height 2))
                                                    (* (rand) (/ eye-height 4)))
                                         :height eye-height
                                         :width  eye-width}
                                  [:fill {:color "black"}
                                   [:rect {:x      (+ (- (/ eye-width 2)
                                                         (/ pupil-width 2))
                                                      (* (/ pupil-width 2) x-offset))
                                           :y      (+ (- (/ eye-height 2)
                                                         (/ pupil-height 2))
                                                      (* (/ pupil-height 2) y-offset))
                                           :height pupil-height
                                           :width  pupil-width}]]]]))]])]]))
      (let [old-key (-> @*state :player :key-pressed)
            pressed-keys (p/get-pressed-keys game)]
        (swap! *state (fn [s]
                        (cond (= old-key :move-executed) (if (empty? pressed-keys)
                                                           (assoc-in s [:player :key-pressed] :none)
                                                           s)
                              (empty? pressed-keys) s
                              ;(pressed-keys 87) (update :text-y dec) ;up
                              (= pressed-keys #{68}) (assoc-in s [:player :key-pressed] :right)
                              ;(pressed-keys 83) (update :text-y inc) ;down
                              (= pressed-keys #{65}) (assoc-in s [:player :key-pressed] :left)
                              :default (assoc-in s [:player :key-pressed] :none))))))))

#_(if (= (first body) [x y])
    (let [direction (-> state :player :direction)]
      [:fill {:color "red"}
       [:rect {:x      (+ (* x tile-width)
                          (/ tile-width 3))
               :y      (+ (* y tile-height)
                          (/ tile-height 3))
               :width  tile-width
               :height tile-height}]]))

#_[:fill {:color "black"}
   [:text {:value (str "Hello, world!" (p/get-pressed-keys game))
           :x     (:text-x state)
           :y     (:text-y state)
           :size  16
           :font  "Georgia"
           :style :italic}]]

(def menu-screen
  (reify p/Screen

    ; runs when the screen is first shown
    (on-show [this])

    ; runs when the screen is hidden
    (on-hide [this])

    ; runs every time a frame must be drawn (about 60 times per sec)
    (on-render [this]
      (p/render game
                [[:fill {:color "lightblue"}
                  [:rect {:x 0 :y 0 :width 500 :height 500}]]
                 [:fill {:color "black"}
                  [:text {:value (str "press any button to start!")
                          :x     (/ (:width @*state) 4)
                          :y     (/ (:height @*state) 2)
                          :size 16
                          :font "Georgia"
                          :style :italic}]]])
      (let [pressed-keys (p/get-pressed-keys game)]
        (if (empty? pressed-keys)
          nil
          (p/set-screen game main-screen))))))


; start the game
(doto game
  (p/start)
  (p/set-screen menu-screen))