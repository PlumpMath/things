(ns things.core
  (:require
   [reagent.core :as r]
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

(defn main []
  [:div
   (simple-circles 300 40 80 20)
   [:hr]
   (shapes 600 200 200 (shape 3 60) "rebeccapurple")
   [:hr]
   [:div {:style {:background "rgb(40,40,40)"}}
    (shapes 600 200 200 (star-shape 3 60 20) "rgba(255,255,0,0.2)")]
   [:hr]])

(defn ^:export on-js-reload []
  (r/render [main]
            (js/document.getElementById "app")))
