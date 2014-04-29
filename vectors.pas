unit vectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Vector3D=Array[0..2]of double;
  Matrix3D= array[0..2,0..2] of double;


  function Magnitude2(const v:Vector3D):double;
  function Distance2(const x,y:Vector3D):double;
  procedure minus(const x,y:Vector3D;var res:Vector3D);
  procedure plus(const x,y:Vector3D;var res:Vector3D);
  procedure plusFactor(const x,y:Vector3D;f:double;var res:Vector3D);
  procedure Middle(const x,y:Vector3D;out res:Vector3D);
  procedure add(var x:Vector3D;const y:Vector3D);
  procedure factor(const x:Vector3D;var res:Vector3D;f:double);
  function inproduct(const x,y:Vector3D):double;
  procedure CrossProduct(const x,y:Vector3D;out res:Vector3D);
  procedure normalize(var x:Vector3D);
  procedure VectorBounds(const v:Vector3D;var min,max:Vector3d);
  procedure SetVector(out v:Vector3D;x,y,z:double);
  procedure ResetVector(out v:Vector3D);
  function power(base,exponent:double):double;

  function transformRow(const m:Matrix3D;const v:Vector3D;row:integer):double;
  procedure RotateMatrix(const m:matrix3D;const angle:double;axis:integer;out res:matrix3D);


implementation

function Magnitude2(const v:Vector3D):double;
begin
  result:= sqr(v[0])+sqr(v[1])+sqr(v[2]);
end;
function Distance2(const x,y:Vector3D):double;
begin
  result:= sqr(x[0]-y[0])+sqr(x[1]-y[1])+sqr(x[2]-y[2]);
end;

procedure minus(const x,y:Vector3D;var res:Vector3D);
var i:integer;
begin
  for i:= 0 to 2 do
    res[i]:= x[i]-y[i];
end;
procedure plus(const x,y:Vector3D;var res:Vector3D);
var i:integer;
begin
  for i:= 0 to 2 do
    res[i]:= x[i]+y[i];
end;
procedure plusFactor(const x,y:Vector3D;f:double;var res:Vector3D);
var i:integer;
begin
  for i:= 0 to 2 do
    res[i]:= x[i]+(y[i]*f);
end;
procedure Middle(const x,y:Vector3D;out res:Vector3D);
var i:integer;
begin
  for i:= 0 to 2 do
    res[i]:= (x[i]+y[i])*0.5;
end;

procedure add(var x:Vector3D;const y:Vector3D);
var i:integer;
begin
  for i:= 0 to 2 do
    x[i]:= x[i]+y[i];
end;
procedure factor(const x:Vector3D;var res:Vector3D;f:double);
var i:integer;
begin
  for i:= 0 to 2 do
    res[i]:= x[i]*f;
end;
function inproduct(const x,y:Vector3D):double;
begin
    result:= x[0]*y[0]+x[1]*y[1]+x[2]*y[2];
end;
procedure CrossProduct(const x,y:Vector3D;out res:Vector3D);
begin
  res[0]:= x[1]*y[2]-x[2]*y[1];
  res[1]:= x[2]*y[0]-x[0]*y[2];
  res[2]:= x[0]*y[1]-x[1]*y[0];
end;

procedure normalize(var x:Vector3D);
var f:double;
begin
  f:= Magnitude2(x);
  if f<>0
    then f:= 1/sqrt(f)
    else f:= 0;
  factor(x,x,f);
end;

function transformRow(const m:Matrix3D;const v:Vector3D;row:integer):double;
begin
  result:= m[0,row]*v[0]+ m[1,row]*v[1]+ m[2,row]*v[2];
end;

procedure VectorBounds(const v:Vector3D;var min,max:Vector3d);
var i:integer; d:double;
begin
  for i:=0 to 2 do
  begin
    d:= v[i];
    if d<min[i] then min[i]:= d;
    if d>max[i] then max[i]:= d;
  end;
end;

procedure SetVector(out v:Vector3D;x,y,z:double);
begin
  v[0]:= x;
  v[1]:= y;
  v[2]:= z;
end;

procedure ResetVector(out v:Vector3D);
begin
  v[0]:= 0;
  v[1]:= 0;
  v[2]:= 0;
end;

function power(base,exponent:double):double;
begin
  result:= Exp(exponent*Ln(base));
end;

procedure RotateMatrix(const m:matrix3D;const angle:double;axis:integer;out res:matrix3D);
var sx,cx:double;
var a,b,c:integer;
begin
  sx:= sin(angle);
  cx:= cos(angle);
  if (axis=0) then
  begin
    a:= 2;
    b:= 0;
    c:= 1;
  end
  else if (axis=1) then
  begin
    a:= 0;
    b:= 1;
    c:= 2;
  end
  else
  begin
    a:= 1;
    b:= 2;
    c:= 0;
  end;
  res[0,a]:= m[0,a]*cx + m[0,c]*sx;
  res[0,b]:= m[0,b];
  res[0,c]:= m[0,c]*cx - m[0,a]*sx;
  res[1,a]:= m[1,a]*cx + m[1,c]*sx;
  res[1,b]:= m[1,b];
  res[1,c]:= m[1,c]*cx - m[1,a]*sx;
  res[2,a]:= m[2,a]*cx + m[2,c]*sx;
  res[2,b]:= m[2,b];
  res[2,c]:= m[2,c]*cx - m[2,a]*sx;
end;

end.

