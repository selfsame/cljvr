(ns game.assets
  (use arcadia.linear)
  (import [UnityEngine Mathf Mesh Quaternion Vector3]))

(import [UnityEditor AssetDatabase])

(def twopi (* Mathf/PI 2))

(defn vert-ring [n r point dir]
  (let [step (/ twopi n)]
    (map 
      (fn [i]
        (let [u (* i step)]
          (v3+ 
            point
            (q* dir 
                (v3 (* r (Mathf/Sin u))
                    (* r (Mathf/Cos u)) 0.0)))))
      (range n))))

(defn ring-tris [n i vs]
  (mapcat
    (fn [idx]
      (let [base (* n i)]
        (if (= idx (dec n))
          [(+ base idx)(+ base idx n)(+ base)
           (+ base idx n)(+ base n)(+ base)]
          [(+ base idx)(+ base idx n)(+ base idx 1)
           (+ base idx n)(+ base idx n 1)(+ base idx 1)])))
    (range n)))

(defn tube-asset [radius n s]
  (let [mesh (Mesh.)
        vs (mapcat 
            #(let [_ %] 
              (vert-ring n 1 
                (v3 0 0 (* % 0.0001))
                (Quaternion. 0 0 0 1))) 
             (range s))
        ts (mapcat #(ring-tris n % vs) (range (dec s)))
        ]
      (set! (.vertices mesh) (into-array Vector3 vs ))
      (set! (.triangles mesh) (into-array System.Int32 ts ))
      (.RecalculateNormals mesh)
      (set! (.name mesh) "tube-mesh")
      mesh))


'(AssetDatabase/CreateAsset (tube-asset 1 12 10) "Assets/tube-mesh.asset")
'(AssetDatabase/SaveAssets)
'(AssetDatabase/Refresh)