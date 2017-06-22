using UnityEngine;
using System.Collections;

public static class Snek
{
    public static void PlaceTube(int offset, GameObject rawtube, GameObject tube, Vector3[] vs, Quaternion[] ds)
    {
        Vector3[] verts = rawtube.GetComponent<MeshFilter>().mesh.vertices;
        int last_idx = verts.Length - 1;
        for (int i = 0; i < 10; i++)
        {
            int valid_idx = Mathf.Min(i + offset, last_idx);
            Vector3 v = vs[valid_idx];
            Quaternion d = ds[valid_idx];
            for (int j = 0; i < 12; j++)
            {
                int idx = Mathf.Min( (9 - i) * 12 + j,  verts.Length-1);
                verts[idx] = (d * verts[idx] * 0.05f) + v;
            }
        }
        tube.GetComponent<MeshFilter>().mesh.vertices = verts;
    }

}