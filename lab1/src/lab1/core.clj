(ns lab1.core
  (:gen-class))
;;подключение библиотек
(use 'clojure.java.io)
(use 'clojure.string)
(use 'clojure.math.numeric-tower)
;; объявление констант
(def ^:const rad1 0.5)
(def ^:const rad2 (* 1.5 rad1))
(def ^:const highestE 0.5)
(def ^:const lowestE 0.15)

(require '[clojure.data.csv])

(defn a []
  (/ 4 (* rad1 rad1)))

(defn b []
  (/ 4 (* rad2 rad2)))

(defn matrixTranspose [matrix]
    (vec (apply map list matrix)))

(defn normVec [v]
  (let [maxValue (double (apply max v))];;ищем макс и рез применяем к maxvalue
    (vec (map (fn[value] (/ value maxValue)) v))));;расчет рез-та и запись в v.

(defn getEvklidRasst [tochka1 tochka2]
  (map (fn[p1, p2] (* (- p1 p2) (- p1 p2))) tochka1 tochka2))

(defn getHemmRast [tochka1 tochka2]
  (map (fn[p1, p2] (java.lang.Math/abs (- p1 p2))) tochka1 tochka2))

(defn getDistance [tochka1 tochka2 distanceFunction]
  (reduce + (distanceFunction tochka1 tochka2)));; суммирует первое со вторым, потом этот рез-т с 3им и тд

(defn findADistanse [distanceType];; выбор алгоритма
  (case distanceType
    1 getEvklidRasst
    2 getHemmRast
    nil))

(defn getPotentialItem [tochka1 tochka2 distanceFunc koef]
  (expt (. Math E) (* (- koef) (getDistance tochka1 tochka2 distanceFunc))))

(defn getPotential [point tochki distanceFunc koef]
  (reduce + (map (fn[pointj] (getPotentialItem point pointj distanceFunc koef)) tochki)))


(defn getCSV;;достаем из файла значения
  [fname firstCol lasrCol]
(map (fn[val] ( map (fn[value] (+ (read-string value) 3)) (subvec val firstCol lasrCol)));;subvec вернет вектор айтемов начиная с первой колонки до последней

 (with-open [file (reader fname)]
  (doall
     (csv/read-csv file))))
  )

( getCSV "стекло.txt" 1 11)
(defn isKlastCenter [firstPotential potential point centers distFunc]
  (if (> potential (* highestE firstPotential))
    true
    (if (< potential (* lowestE firstPotential))
      false
      (let [minDistance (apply min (map (fn[pt] (getDistance point pt distFunc)) centers))];; поиск мин дистанции
        (if (< 1 (+ (/ minDistance rad1) (/ potential firstPotential)))
          true
          nil
          )
        )
      )
    )
  )

(defn pereschetPot [potentials tochki maxPotential center distFunc koef]
  (vec (map (fn[potential point] (- potential (* maxPotential (getPotentialItem point center distFunc koef)))) potentials tochki)))

(defn -main[path alg  firstCol lasrCol]
  (let [[tochki (matrixTranspose (map (fn[pt] (normVec pt)) (matrixTranspose (getCSV path firstCol lasrCol))))] [distFunc (findADistanse alg)]]
   (let [[potentials (map (fn[pt] (getPotential pt tochki distFunc (a))) tochki)][firstPotential (double (apply max potentials))]]
      (loop [potentials potentials centers [] finished false];;цикл
              (when (not finished)
                (let [[[[maxPotential (apply max potentials)][maxPotentialIndex (.indexOf potentials maxPotential)]]
                    [centerPoint (get tochki maxPotentialIndex)]][isCenter (isKlastCenter firstPotential maxPotential centerPoint centers distFunc)]]
                        (if (boolean isCenter)
                          (println maxPotentialIndex (get tochki maxPotentialIndex)))
                        (if (nil? isCenter)
                          (recur (assoc potentials maxPotentialIndex 0) centers false);;возвращается в начало цикла с новыми параметрами. возвращается map с присвоение 0
                          (recur
                           (pereschetPot potentials tochki maxPotential centerPoint distFunc (b))
                           (conj centers centerPoint);;добавление айтем в коллекцию
                           (not isCenter))
                          )
                        )
                      )
                    )
                  )
                )
  )

(-main "стекло.txt" 2 0 11)



























