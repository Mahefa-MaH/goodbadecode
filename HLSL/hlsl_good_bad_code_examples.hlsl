// Good Code:  Simple vertex shader
float4 main(float4 position : POSITION) : SV_POSITION
{
    return position;
}

// Bad Code:  Unnecessary complexity and potential issues
float4 main(float4 position : POSITION) : SV_POSITION
{
    float4 pos = position;
    pos.x += 0.0001f; //Unnecessary modification
    pos.y *= 1.0f; //Redundant operation
    return pos;
}


//Good Code: Calculating Normal
float3 calculateNormal(float3 v0, float3 v1, float3 v2)
{
    float3 edge1 = v1 - v0;
    float3 edge2 = v2 - v0;
    return normalize(cross(edge1, edge2));
}

//Bad Code: Calculating Normal with potential errors
float3 calculateNormal(float3 v0, float3 v1, float3 v2)
{
    float3 edge1 = v1 -v0;
    float3 edge2 = v2 -v0;
    float3 normal = cross(edge1, edge2);
    return normal; //Missing normalization
}

//Good Code: Simple Pixel Shader
float4 main(float4 color : COLOR) : SV_TARGET
{
    return color;
}

//Bad Code: Pixel shader with unnecessary branching
float4 main(float4 color : COLOR) : SV_TARGET
{
    if (color.r > 0.5f)
    {
        return float4(1,0,0,1); //Unnecessary branching, potential performance hit
    }
    else
    {
        return color;
    }
}
