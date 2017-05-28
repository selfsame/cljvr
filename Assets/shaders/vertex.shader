Shader "Vertex Colored"
{
    Properties
    {
        _Emission("Emmisive Color", Color) = (0,0,0,0)
    }
 
    SubShader
    {
        Pass
        {

 
            
            Lighting Off
        }
    }
    Fallback "VertexLit", 1
}