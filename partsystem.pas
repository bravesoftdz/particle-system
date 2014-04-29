unit partsystem;

{$mode objfpc}{$H+}

interface

uses vectors,classes;

type

  CalcParams=record
    noEM:boolean;
    linear:boolean;
    accuracy:double;
    minStep:double;
    maxStep:double;
  end;

  { Particle }
  Particle=class
    id:string;
    color:integer;
    pos,speed:Vector3D;
    restmass,electric_charge,radius:double;
    rest_magnetic_impulse:Vector3D; // related to spin
    ferromagnetic:double; // attracts and attracted by magnetism
    dielectric:double; // attracts and attracted by charges
    //generated:
    newPos,newSpeed:Vector3D;
    nearestMagnitudeSet:boolean;
    nearestMagnitude2:double;
    acc:Vector3D;
    magnetic_direction:Vector3D;
    total_force:Vector3D;
    g_field,e_field,m_field:Vector3D;
    d_field,f_field:Vector3D;
    invRestmass:double;
    procedure ClearFields;
    procedure AddList(particles:TList;const params:CalcParams);
    procedure AddFields(const p:Particle;const params:CalcParams);
    procedure MergeField(const p:Particle);
    procedure Randomize;
    procedure ApplyForce(time:double;const params: CalcParams);
    procedure PrepareForce(change:double;out timeNeeded:double;const params: CalcParams);
    procedure Update;
  end;
  collisionTypes=(ct_bounce,ct_cling,ct_merge,ct_break);

  { ParticleCell }

  ParticleCell=class
    //dynamic:
    particles:TList;
    center:Vector3D;
    //generated:
    replaceParticle:Particle;
    replaceDistance:double;
    procedure GetForceForAll(allCells:TList;params:CalcParams);
    function CellIsFar(other:ParticleCell):boolean;

    procedure PrepareReplace(param:CalcParams);
    // recalculate the particle replacement with statistics
    procedure GetForceForReplacement(allCells:TList;param:CalcParams);

  end;

  { CellBounds }

  CellBounds= class
    min,max:double;
    count:integer;
    circular:boolean;
    cellMin,cellMax:ParticleCell;
    factor:double;
    procedure Prepare;
    function GetIndexOrCell(v:double;out index:integer):ParticleCell;
  private
    procedure GetCellBounds(c,ix: integer; var vmin, vmax, pmin,pmax: Vector3D);
  end;

  { CellGrid }

  CellGrid=class
    AllParticles:TList;
    AllCells:TList;
    params:CalcParams;
    boundsX,boundsY,boundsZ:CellBounds;
    procedure MakeGrid(count: integer; min,max:Vector3D; cx, cy, cz: boolean);
    procedure RecalculateGrid;
      // get moved particles in the correct cell
    function GetCellAt(v:Vector3d):ParticleCell;
    procedure CalcForcesForAll;
    procedure PrepareForceForAll(change:double;out timeNeeded:double);
    procedure ApplyForcesForAll(timeStep:double);
    procedure CalcParticles(maxTimeStep:double);
    procedure ReGridForParticles(extra: double);
    procedure RecalcCellBounds(min,max:Vector3D);
  end;



implementation

uses
  SysUtils;

{ CellGrid ------------------------------------- }

procedure CellGrid.ReGridForParticles(extra:double);
var i:integer; min,max:Vector3D; p:Particle;
  procedure SetBounds(ix:integer;bounds:CellBounds);
  var dist:double;
  begin
    if not bounds.circular then
    begin
      dist:= (max[ix]-min[ix])*extra;
      bounds.min:= min[ix]-dist;
      bounds.max:= max[ix]+dist;
    end;
  end;

begin
  for i:= 0 to AllParticles.count-1 do
  begin
    p:= Particle(AllParticles[i]);
    if i=0 then
      begin
        min:= p.pos;
        max:= p.pos;
      end
    else VectorBounds(p.pos,min,max);
  end;
  SetBounds(0,BoundsX);
  SetBounds(1,BoundsX);
  SetBounds(2,BoundsX);
  RecalcCellBounds(min,max);
end;

procedure CellGrid.RecalcCellBounds(min,max:Vector3D);
  procedure SetBounds(ix:integer;var b:CellBounds);
  var offs:double;
  begin
    b.min:= min[ix];
    b.max:= max[ix];
    if not b.circular then
    begin
      b.cellMin:= ParticleCell.create;
      b.cellMax:= ParticleCell.create;
      Middle(min,max,b.cellMin.Center);
      b.cellMax.center:= b.cellMin.center;
      offs:= (max[ix]-min[ix])/b.count;
      b.cellMin.center[ix]:= min[ix]-offs;
      b.cellMax.center[ix]:= min[ix]+offs;
    end;
  end;
  var xx,yy,zz,ix:integer; pMin,pMax:Vector3D; cell:ParticleCell;
begin
  SetBounds(0,boundsX);
  SetBounds(1,boundsY);
  SetBounds(2,boundsZ);

  ix:= 0;
  for zz:= 0 to boundsZ.count-1 do
  for yy:= 0 to boundsY.count-1 do
  for xx:= 0 to boundsX.count-1 do
  begin
    BoundsX.GetCellBounds(0,xx,min,max,pMin,pMax);
    BoundsY.GetCellBounds(1,yy,min,max,pMin,pMax);
    BoundsZ.GetCellBounds(2,zz,min,max,pMin,pMax);
    cell:= ParticleCell(AllCells[ix]);
    ix:= ix+1;
    Middle(pMin,pMax,cell.Center);
  end;
end;

procedure CellGrid.MakeGrid(count: integer; min,max:Vector3D; cx, cy, cz: boolean);
var c:integer; cell:ParticleCell;
    xx,yy,zz:integer; pMin,pMax:Vector3D;

begin
  boundsX:= CellBounds.create;
  boundsY:= CellBounds.create;
  boundsZ:= CellBounds.create;
  boundsX.circular:= cx;
  boundsY.circular:= cy;
  boundsZ.circular:= cz;
  boundsX.count:= count;
  boundsY.count:= count;
  boundsZ.count:= count;
  for zz:= 0 to count-1 do
  for yy:= 0 to count-1 do
  for xx:= 0 to count-1 do
  begin
    cell:= ParticleCell.create;
    self.AllCells.add(cell);
  end;
  RecalcCellBounds(min,max);
end;

procedure CellGrid.RecalculateGrid;
var cell:ParticleCell;
    c:integer; p:Particle;
begin
  boundsX.prepare;
  boundsY.prepare;
  boundsZ.prepare;
  // prepare cells
  for c:= 0 to AllCells.count-1 do
  begin
    cell:= ParticleCell(AllCells[c]);
    cell.particles.clear;
  end;
  // place particles in right cells:
  for c:= 0 to AllParticles.count-1 do
  begin
    p:= Particle(AllParticles[c]);
    cell:= GetCellAt(p.pos);
    cell.particles.add(p);
  end;
end;

function CellGrid.GetCellAt(v: Vector3d): ParticleCell;
var xx,yy,zz,ix:integer; cell:ParticleCell;
begin
  cell:= BoundsX.GetIndexOrCell(v[0],xx);
  if cell<>nil
    then begin result:= cell; EXIT; end;
  cell:= BoundsY.GetIndexOrCell(v[1],yy);
  if cell<>nil
    then begin result:= cell; EXIT; end;
  cell:= BoundsZ.GetIndexOrCell(v[2],zz);
  if cell<>nil
    then begin result:= cell; EXIT; end;
  ix:= xx+boundsX.count*(yy+zz*BoundsY.count);
  if (ix>=0) and (ix<AllCells.count)
    then cell:= ParticleCell(AllCells[ix])
    else Raise exception.create('Calculation failed for grid.');
end;


procedure CellGrid.CalcForcesForAll;
var cell:ParticleCell;
    c:integer;
begin
  // prepare cells
  for c:= 0 to AllCells.count-1 do
  begin
    cell:= ParticleCell(AllCells[c]);
    cell.PrepareReplace(params);
  end;
  // calculate replacements within cells
  for c:= 0 to AllCells.count-1 do
  begin
    cell:= ParticleCell(AllCells[c]);
    cell.GetForceForReplacement(allCells,params);
  end;
  // calculate forces of particles within cells
  for c:= 0 to AllCells.count-1 do
  begin
    cell:= ParticleCell(AllCells[c]);
    cell.GetForceForAll(allCells,params);
  end;
end;

procedure CellGrid.PrepareForceForAll(change: double; out timeNeeded: double);
var i:integer; p:Particle; timeN:double;
begin
  timeNeeded:= 0;
  for i:= 0 to allParticles.count-1 do
  begin
    p:= Particle(AllParticles[i]);
    p.PrepareForce(change,timeN,params);
    if i=0
      then timeNeeded:= timeN
      else if timeN>timeNeeded then timeNeeded:= timeN;
  end;
end;

procedure CellGrid.ApplyForcesForAll(timeStep: double);
var i:integer; p:Particle;
begin
  for i:= 0 to AllParticles.count-1 do
  begin
    p:= Particle(AllParticles[i]);
    p.ApplyForce(timeStep,params);
    p.Update;
  end;

end;

procedure CellGrid.CalcParticles(maxTimeStep:double);
var i:integer;
    timeStep:double;
    timeNeeded:double;
    p:Particle;
    cell:ParticleCell;
begin
  RecalculateGrid;
  for i:= 0 to Allparticles.count-1 do
  begin
    p:= Particle(AllParticles[i]);
    p.ClearFields;
  end;

  CalcForcesForAll;
  self.PrepareForceForAll(0.1,timeNeeded);
  if timeNeeded>maxTimeStep
    then timeStep:= maxTimeStep
    else timeStep:= timeNeeded;
  ApplyForcesForAll(TimeStep);
end;

{ CellBounds ------------------------ }

procedure CellBounds.prepare;
begin
  factor:= 1/(max-min);
end;

function CellBounds.GetIndexOrCell(v:double;out index:integer):ParticleCell;
var x:double; xx:integer;
begin
  index:= 0;
  result:= nil;
  x:= (v-min)*factor;
  if (x<1) and (x>=0) then
    xx:= round(trunc(x*count))
    else
    if Circular
      then xx:= round(trunc(Frac(x)*count))
      else
        begin
          if x<0
            then result:= cellMin
            else result:= cellMax;
          EXIT;
        end;
  index:= xx;
end;

procedure CellBounds.GetCellBounds(c,ix: integer; var vmin, vmax, pmin,pmax: Vector3D);
var dmin,dmax:double;
begin
  dmin:= vmin[c];
  dmax:= vmax[c];
  pmin[c]:= (ix/(count+1))*(dmax-dmin)+ dmin;
  pmax[c]:= ((ix+1)/(count+1))*(dmax-dmin)+ dmin;
end;

{ ParticleCell ------------------------ }

const ReplaceFactor=4;

procedure ParticleCell.PrepareReplace(param:CalcParams);
var i:integer; p:Particle; variation,d2,dist:double;
begin
  if replaceParticle=nil
    then replaceParticle:= Particle.create;
  variation:= 0;
  replaceParticle.pos:= center;
  replaceParticle.restmass:=0;
  replaceParticle.electric_charge:=0;
  replaceParticle.ClearFields;
  for i:= 0 to particles.count-1 do
  begin
    p:= Particle(particles[i]);
    replaceParticle.restmass:= replaceParticle.restmass+p.restmass;
    replaceParticle.electric_charge:= replaceParticle.electric_charge+p.electric_charge;
    // magnetism needs something else...
    //....
    // variation calculation:
    d2:= distance2(replaceParticle.pos,p.pos);
    if variation<d2 then d2:= variation;
  end;
  dist:= sqrt(variation);
  replaceParticle.radius:= dist;
  replaceDistance:= dist*ReplaceFactor;
    // replacement factor (=4).. everything further will be averaged to cell statistics
end;
procedure ParticleCell.GetForceForReplacement(allCells:TList;param:CalcParams);
var c:integer; cell:ParticleCell;
begin
  for c:= 0 to allCells.count-1 do
  begin
    cell:= ParticleCell(allCells[c]);
    if self<>cell then
    if self.cellIsFar(cell) then
    begin
      self.replaceParticle.AddFields(cell.replaceParticle,param);
    end;
  end;
end;

function ParticleCell.CellIsFar(other:ParticleCell):boolean;
var radiusSum2,dist2:double;
begin
  result:= true;
  if other<>self then
  begin
    dist2:= Distance2(self.replaceParticle.pos,other.replaceParticle.pos);
    radiusSum2:= sqr(self.replaceDistance+other.replaceDistance);
    if dist2<radiusSum2 then
    begin
      // check all particles for this cell
      result:= false;
    end;
  end;
end;

procedure ParticleCell.GetForceForAll(allCells:TList;params:CalcParams);
var i,c,s:integer; cell:ParticleCell; p:Particle; dist2:double; skip:boolean;
begin
  for c:= 0 to allCells.count-1 do
  begin
    cell:= ParticleCell(allCells[c]);
    skip:= cellIsFar(cell);
    if not skip then
    for i:= 0 to particles.count-1 do
      begin
        p:= Particle(particles[i]);
        for s:= 0 to cell.particles.count-1 do
        begin
          p.AddFields(Particle(cell.particles[s]),params);
        end;
      end;
    end;
  for i:= 0 to particles.count-1 do
  begin
    p:= Particle(particles[i]);
    p.MergeField(self.replaceParticle);
  end;
end;


{ Particle ------------------------- }

procedure Particle.ClearFields;
var i:integer;
begin
  nearestMagnitudeSet:= false;
  nearestMagnitude2:= 0;
  ResetVector(g_field);
  ResetVector(e_field);
  ResetVector(m_field);
  ResetVector(total_force);
  ResetVector(acc);
  ResetVector(newPos);
  ResetVector(newSpeed);
  invRestmass:= 1.0/self.restmass;
  //ResetVector(d_field);
  //ResetVector(f_field);
  PlusFactor(self.rest_magnetic_impulse,speed,electric_charge,magnetic_direction);
end;

procedure Particle.AddList(particles: TList; const params: CalcParams);
var i:integer; p:Particle;
begin
  for i:= 0 to particles.count-1 do
  begin
    p:= Particle(particles[i]);
    if self<>p
      then self.AddFields(p,params);
  end;
end;

const G_Const= 6.6738E-11; // gravity constant
const E_Const= 8.98755E9; // electric constant
const C_Const= 600000000; // speed of light
const M_Const= E_Const/(C_Const*C_Const); // magnetic constant

procedure Particle.AddFields(const p: Particle; const params: CalcParams);
var diff,diff_i3,temp_:Vector3D;
    distance_2,mindist_2,distance_i3,distance_i2,distance:double;
    fac:double;
    collision_type:collisionTypes;
begin
  minus(pos,p.pos,diff);
  distance_2:= Magnitude2(diff);
  mindist_2:= sqr(radius+p.radius);
  if distance_2>mindist_2
    then
      begin
        distance_i2:= 1/distance_2;
        distance:= sqrt(distance_2);
        distance_i3:= distance*distance_i2*distance_i2;
        if not nearestMagnitudeSet
          then
            begin
              nearestMagnitude2:= distance_2;
              nearestMagnitudeSet:= true;
            end
          else if distance_2<nearestMagnitude2
          then
            begin
              nearestMagnitude2:= distance_2
            end;
      end
    else
      begin //collision
        distance_i2:= 1/mindist_2;
        distance:= sqrt(mindist_2);
        distance_i3:= distance*distance_i2*distance_i2;
        collision_type:= ct_bounce;
        case collision_type of
          ct_bounce:begin end;
          ct_cling:begin end;
          ct_merge:begin end;
          ct_break:begin end;
        end;
      end;
  Factor(diff,diff_i3,distance_i3);
  plusFactor(g_field,diff_i3,(-G_Const*p.restmass),g_field);
  if params.noEM then
  begin
  end
  else
  begin
    plusFactor(e_field,diff_i3,(E_Const*p.electric_charge),e_field);
    CrossProduct(p.speed,diff_i3,temp_);
    plusFactor(m_field,temp_,(M_Const*p.electric_charge),m_field);
    //plusFactor(d_field,diff_i3,(E_Const*p.dielectric),d_field);
    //plusFactor(f_field,diff_i3,(M_Const*p.ferromagnetic),f_field);
    //plusFactor(fm_field,diff_i3,(M_Const*p.speed*p.electric_charge),fm_field);
  end;
end;

procedure Particle.MergeField(const p: Particle);
begin
  Plus(g_field,p.g_field,g_field);
  Plus(e_field,p.e_field,e_field);
  Plus(m_field,p.m_field,m_field);
  //Plus(d_field,p.d_field,d_field);
  //Plus(f_field,p.f_field,f_field);
end;


procedure Particle.Randomize;
  procedure RandomVector(var v:Vector3D;r:double);
  begin
    v[0]:= random*r*2-r;
    v[1]:= random*r*2-r;
    v[2]:= random*r*2-r;
  end;
begin
  radius := 0.1;
  RandomVector(pos,100);
  SetVector(speed,0,0,0);
  //RandomVector(speed,0.001);
  Restmass := 0.1+ Random*10;
  invRestmass:= 1/Restmass;
  electric_charge := Random*1-0.5;
end;


procedure Particle.PrepareForce(change:double;out timeNeeded:double;const params: CalcParams);
var speedTime,accTime,t,det:double; speedMag,accMag:double;
    maxDistance,maxSpeed:double; m_force:Vector3D;
begin
  if not params.NoEM then
  begin
    PlusFactor(acc,e_field,electric_charge*invRestMass,acc);
    CrossProduct(m_field,speed,m_force);
    PlusFactor(acc,m_force,invRestMass,acc);
    //PlusFactor(acc,d_field,electric_charge*invRestMass,acc);
    //PlusFactor(acc,e_field,dielectric*invRestMass,acc);
  end;
  PlusFactor(acc,g_field,1,acc);

  speedMag:= sqrt(magnitude2(speed));
  accMag:= sqrt(magnitude2(acc));
  maxDistance:= sqrt(nearestMagnitude2);
  maxSpeed:= speedMag;
  if speedMag<10E-20
    then speedMag:= 10E-20;
  if accMag<10E-20
    then accMag:= 10E-20;
  if accMag>10E20
    then accMAg:= 10E20;
  speedTime:= (change*maxDistance)/speedMag;
  accTime:= change*sqrt(maxDistance/accMag);
  if speedTime<accTime
    then timeNeeded:= speedTime
    else timeNeeded:= accTime;
end;

procedure Particle.ApplyForce(time:double;const params: CalcParams);
begin
  // f(t)= f+ f'*t+ 0.5*f''*t*t
  // f(t)= pos+ speed*t+ 0.5*acc*t*t
  PlusFactor(pos,speed,time,newPos);
  if not params.linear
    then PlusFactor(newPos,acc,time*time*0.5,newPos);
  PlusFactor(speed,acc,time,newSpeed);
end;

procedure Particle.Update;
begin
  pos:= newPos;
  speed:= newSpeed;
end;

end.

