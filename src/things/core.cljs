(ns things.core
  (:require
   [reagent.core :as r]
   [thi.ng.color.core :as col]
   [thi.ng.geom.core :as g]
   [thi.ng.math.core :as m]
   [thi.ng.geom.core.vector :as v :refer [vec2 vec3]]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.polygon :as poly]
   [thi.ng.geom.svg.core :as svg]
   [thi.ng.geom.svg.adapter :as svgadapt]))

(enable-console-print!)

(defn svg-component [width body]
  [:div
   (->> body
         (svgadapt/all-as-svg)
         (svgadapt/inject-element-attribs svgadapt/key-attrib-injector)
         (svg/svg {:width width :height width}))])

(defn poly-circle
  [{:keys [r sides res width]}]
  (->> (for [pos (-> (c/circle r)
                     (g/as-polygon sides)
                     (g/sample-uniform res false))]
         (c/circle (g/madd pos [1 1] (/ width 2)) 1))
       (svg/group {:stroke "none" :fill "blue"})
       (svg-component width)))

(defn simple-circles
  [width num r1 r2]
  (->> (for [i (butlast (m/norm-range num))
             :let [pos (-> (vec2 r1 (* m/TWO_PI i))
                              (g/as-cartesian)
                              (g/madd [1 0.5] (/ width 2)))]]
            (c/circle pos r2))
       (svg/group {:stroke "blue" :fill "none"})
       (svg-component width)))

(defn shapes
  [width num r1 shape-fn color]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos (-> (vec2 r1 theta)
                           (g/as-cartesian)
                           (g/madd [1 0.5] (/ width 2)))]]
         (shape-fn pos theta))
       (svg/group {:stroke color :fill "none"})
       (svg-component width)))

(defn shape
  [res radius]
  (let [proto (-> (c/circle radius)
                  (g/center)
                  (g/as-polygon res))]
    (fn [pos theta]
      (-> proto
          (g/rotate (* 2 theta))
          (g/translate pos)))))

(defn star-shape
  [res r1 r2]
  (let [proto (poly/cog r2 res [(/ r1 r2) 1])]
    (fn [pos theta]
      (-> proto
          (g/rotate (* 2 theta))
          (g/translate pos)))))

(defn waves
  [{:keys [width num radius shape bg color]}]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos (-> (vec2 (radius theta) theta)
                           (g/as-cartesian)
                           (g/madd [1 1] (/ width 2)))]]
         (shape pos theta))
       (svg/group
        {:stroke (or color "rgba(0,0,255,0.25)") :fill "none"})
       (svg-component width)))

(defn waveform
  [min max freq]
  (let [offset (m/mix min max 0.5)
        delta (- max offset)]
    (fn [theta]
      (+ offset (* delta (Math/sin (* freq theta)))))))

(defn wat
  [{:keys [width num radius shape dist bg color]}]
  (->> (for [i (butlast (m/norm-range num))
             :let [theta (* m/TWO_PI i)
                   pos (-> (vec2 (radius theta) theta)
                           (g/as-cartesian)
                           (g/madd [1 1] (/ width 2)))]]
         (g/sample-uniform (shape pos theta) dist false))
       ;; concatenate all points into single seq
       (mapcat identity)
       ;; represent each as small dot
       (map #(c/circle % 1.5))
       (svg/group
        {:fill (or color "rgba(0, 0, 255, 0.25)") :strike "none"}
        (if bg (svg/rect [0 0] width width {:fill bg})))
       (svg-component width)))

(defn compute-dejong
  "Computes a single DeJong 2d point vector for given params and XY pos."
  [a b c d x y]
  (vec2
   (+ (Math/sin (* a y)) (Math/cos (* (* b x) x)))
   (+ (Math/sin (* (* c x) x)) (Math/cos (* d y)))))

(defn dejong
  [{:keys [width iter a b c d color]}]
  (let [scale (/ width 4)
        center (vec2 (/ width 2))]
    (->> (range iter)
         ;; Iterative system: f(x + 1) = f(f(x))
         (reduce
          (fn [[points [x y]] _]
            (let [pos (compute-dejong a b c d x y)]
              [(conj points (c/circle (g/madd pos scale center) 1)) pos]))
          ['() [(m/random width) (m/random width)]])
         (first)
         (svg/group
          {:fill (or color "rgb(0,0,0)") :stroke "none"})
         (svg-component width))))

(defn main []
  [:div
   (poly-circle {:r 60
                 :sides 6
                 :res 10
                 :width 300})
   [:hr]
   (simple-circles 300 40 80 20)
   [:hr]
   (shapes 600 200 200 (shape 3 90) "rebeccapurple")
   [:hr]
   [:div {:style {:background "rgb(40,40,40)"}}
    (shapes 600 200 200 (star-shape 6 60 20) "rgba(255,255,0,0.2)")]
   [:hr]
   [:div
    (waves {:width 600
            :num 400
            :radius (waveform 100 200 4)
            :shape (shape 3 90)})]
   [:hr]
   [:div {:style {:background "rgb(40, 40, 40)"}}
    (waves {:width 600
            :num 600
            :radius (waveform 50 200 8)
            :shape (star-shape 5 25 90)
            :color "rgba(255, 250, 240, 0.25)"})]
   [:hr]
   [:div {:style {:background "rgb(250, 250, 200)"}}
    (wat {:width 600
          :num 1000
          :radius (waveform 50 200 8)
          :dist 40
          :shape (star-shape 5 25 90)
          :color "rgba(200, 200, 20, 0.3"
          :bg "rgb(250, 250, 200)"})]
   [:hr]
   [:div {:style {}}
    (dejong {:width 600
             :iter 10002
             :a (m/random -3 3)
             :b (m/random -3 3)
             :c (m/random -3 3)
             :d (m/random -3 3)})]])

(defn ^:export on-js-reload []
  (r/render [main]
            (js/document.getElementById "app")))
