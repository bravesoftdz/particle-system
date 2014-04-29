unit partform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, CheckLst, partSystem, setupreader;

type

  { TParticleForm }

  TParticleForm = class(TForm)
    AutoCalc: TCheckBox;
    AutoZoom: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    AccuracyEdit: TLabeledEdit;
    Button4: TButton;
    MaxStepSecs: TComboBox;
    MinStepSecs: TComboBox;
    StepMinEdit: TLabeledEdit;
    StepMaxEdit: TLabeledEdit;
    LinearCheck: TCheckBox;
    NoEMCheck: TCheckBox;
    PartSelection: TLabel;
    ElementsList: TCheckListBox;
    EditCount: TEdit;
    Label2: TLabel;
    LabelIteration: TLabel;
    LabelTime: TLabel;
    IdleTimer1: TIdleTimer;
    LabelTotalTime: TLabel;
    PageControl1: TPageControl;
    Cathegories: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AutoZoomChange(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure EditTimeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinearCheckChange(Sender: TObject);
    procedure NoEMCheckChange(Sender: TObject);
    procedure EditTimeExit(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure CathegoriesChange(Sender: TObject);
    procedure StepMaxEditChange(Sender: TObject);
    procedure StepMaxEditExit(Sender: TObject);
  private
    { private declarations }
    noEM:boolean;
    linearInter:boolean;
    minStep,maxStep,accuracy:double;
    timeStep:double;
    totalTime:double;
    setup:setupData;
  public
    { public declarations }
    minX,minY,maxX,maxY:double;
    minmaxSet:boolean;
    //
    iteration:integer;
    particleList:Tlist;
    grid:CellGrid;
    procedure PaintParticles(Sender:TObject);
    procedure CalcParticles;
    procedure ClearParticles;
    procedure SetupView;
  end;

var
  ParticleForm: TParticleForm;

implementation

{$R *.lfm}

uses displayform, vectors;

const MicroSecs=1E-6;
const SecsPerDay=60*60*24;
const SecsPerYear=365.25*SecsPerDay;

{ TParticleForm }

procedure TParticleForm.ClearParticles;
var i:integer; p:Particle;
begin
  if particleList=nil
  then particleList:= TList.create
  else
    begin
      for i:= 0 to particleList.count-1 do
      begin
        p:= Particle(particleList[i]);
        p.free;
      end;
      particleList.clear;
    end;
end;
procedure TParticleForm.SetupView;
begin
  if viewForm<>nil then
  begin
    viewForm.repaintEvent:= @PaintParticles;
  end;
  PaintParticles(nil);
  iteration:=0;
  totalTime:= 0;
  if ViewForm<>nil then
  begin
    ViewForm.inval:= true;
    if ViewForm.visible=false
      then ViewForm.show;
  end;
end;

procedure TParticleForm.Button1Click(Sender: TObject);
var i:integer; p:Particle;
    count:integer; cath:SetupCath; el:SetupElement;
begin
  // generate random selection:
  ClearParticles;
  count:= StrToInt(EditCount.text);
  particleList.Capacity:=count;
  i:= Cathegories.TabIndex;
  if i<0 then EXIT;
  cath:= SetupCath(Setup.Cathegories[i]);
  for i:= 0 to count do
  begin
    el:= cath.PickRandomElement;
    p:= el.GenerateParticle(cath.multiplier);
    particleList.add(p);
  end;
  SetupView;
end;

procedure TParticleForm.CalcParticles;
var i:integer; p:Particle;
     timeNeeded:double;
     params:CalcParams;
begin
  params.noEM:= noEM;
  params.linear:= linearInter;
  params.accuracy:= accuracy;
  params.maxStep:= maxStep;
  params.minStep:= minStep;
  for i:= 0 to particleList.count-1 do
  begin
    p:= Particle(particleList[i]);
    p.ClearFields;
  end;

  for i:= 0 to particleList.count-1 do
  begin
    p:= Particle(particleList[i]);
    p.AddList(particleList,params);
  end;
  if params.maxStep= 0
    then timeStep:= secsPerDay //1 day
    else timeStep:= params.maxStep;
  for i:= 0 to particleList.count-1 do
  begin
    p:= Particle(particleList[i]);
    p.PrepareForce(params.accuracy,TimeNeeded,params); // calculate time needed for 10% change
    if timeNeeded>0 then
    if TimeNeeded<timeStep then timeStep:= TimeNeeded;
  end;
  if params.minStep<>0 then
    if timeStep<params.minStep then timeStep:= params.minStep;
  for i:= 0 to particleList.count-1 do
  begin
    p:= Particle(particleList[i]);
    p.ApplyForce(TimeStep,params);
    p.Update;
  end;

end;


procedure TParticleForm.Button2Click(Sender: TObject);
var startTime:TDateTime; elapsed:TDateTime; TotTime:double; TimeMeas:string;
begin
  if particleList=nil then EXIT;

  startTime:= now;
  elapsed:= 0;
  repeat

    CalcParticles;
    elapsed:= elapsed+(now-startTime)*24*60*60;
    LabelTime.Caption:= 'Calculation time='+FloatToStr(elapsed);

    //PaintParticles(nil);
    iteration:= iteration+1;
    totalTime:= totalTime+ timeStep;
    LabelIteration.caption:= 'Step='+IntToStr(iteration);

  until (elapsed>0.1)or(AutoCalc.checked=false);
  if totalTime>SecsPerDay then
    if totalTime>SecsPerYear then
    begin
      TotTime:= totalTime/SecsPerYear;
      timeMeas:= 'Years';
    end
    else
    begin
      TotTime:= totalTime/SecsPerDay;
      timeMeas:= 'Days';
    end
  else
  begin
    TotTime:= totalTime;
    timeMeas:= 'Secs';
  end;
  LabelTotalTime.caption:= 'Total time='+FloatToStr(TotTime)+' '+timeMeas;

  if ViewForm<>nil then
  begin
    ViewForm.inval:= true;
    if ViewForm.visible=false
      then ViewForm.show;
  end;
end;

procedure TParticleForm.AutoZoomChange(Sender: TObject);
begin

end;

procedure TParticleForm.Button3Click(Sender: TObject);
  // generate particles:
  var i:integer; p:Particle;
    sum:double; cath:SetupCath; el,multi:SetupElement;
begin
  ClearParticles;
  i:= Cathegories.TabIndex;
  if i<0 then EXIT;
  cath:= SetupCath(Setup.Cathegories[i]);
  multi:= cath.multiplier;
  sum:= 0;
  i:=0;
  while i<cath.elements.count do
  begin
    el:= SetupElement(cath.elements[i]);
    if sum<el.Amount then
    begin
      p:= el.GenerateParticle(multi);
      particleList.add(p);
      sum:= sum+1;
    end
    else
    begin
      i:= i+1;
      sum:= 0;
    end;
  end;
  SetupView;

end;
procedure TParticleForm.Button4Click(Sender: TObject);
  function ListToSecs(edit:TLabeledEdit;listIx:integer):double;
  var err:integer; v,fact:double;
  begin
    val(edit.text,v,err);
    if err=0 then
    begin
      fact:= 0;
      case listIX of
        0:fact:= MicroSecs;
        1:fact:= 1;
        2:fact:= SecsPerDay;
        3:fact:= SecsPerYear;
      end;
      edit.text:= FloatToStr(v);
      result:= v*fact;
    end
    else result:= 0;
  end;
var err:integer; v:double;
begin
  maxStep:= ListToSecs(StepMaxEdit,MaxStepSecs.itemIndex);
  minStep:= ListToSecs(StepMinEdit,MinStepSecs.itemIndex);
  val(AccuracyEdit.text,v,err);
  if err=0 then accuracy:= v;
  AccuracyEdit.text:= FloatToStr(accuracy);
end;

procedure TParticleForm.EditTimeChange(Sender: TObject);
begin

end;

function GetFullPath(s:string):string;
var x:string; p:integer;
begin
  x:= application.ExeName;
  result:= copy(x,1,length(x)-13)+s;
end;

procedure TParticleForm.FormCreate(Sender: TObject);
var fs:TFileStream; fn:string; i:integer; c:SetupCath;
begin
  formatSettings.DecimalSeparator:= '.';
  //read setupdata.txt
  fn:= GetFullPath('setupdata.txt');
  if fileExists(fn) then
  begin
    fs:= TFileStream.create(fn,fmOpenRead);
    setup:= ReadFromStream(fs);
    fs.free;
    Cathegories.Tabs.clear;
    if setup.cathegories.count=0
      then Cathegories.tabs.Add('No items found');
    for i:= 0 to setup.Cathegories.count-1 do
    begin
      c:= SetupCath(setup.cathegories[i]);
      cathegories.tabs.AddObject(c.name,c);
    end;
    CathegoriesChange(nil);
  end;
end;

procedure TParticleForm.LinearCheckChange(Sender: TObject);
begin
  linearInter:= LinearCheck.checked;
end;

procedure TParticleForm.NoEMCheckChange(Sender: TObject);
begin
  noEM:= NoEMCheck.checked;
end;

procedure TParticleForm.EditTimeExit(Sender: TObject);
begin
end;

procedure TParticleForm.IdleTimer1Timer(Sender: TObject);
begin
  if AutoCalc.checked then Button2Click(nil);
end;

procedure TParticleForm.CathegoriesChange(Sender: TObject);
var ix,i:integer; cath:SetupCath; el:SetupElement;
begin
  // load tabs..
  if setup<>nil then
  begin
    elementsList.items.clear;
    ix:= Cathegories.TabIndex;
    if ix>=setup.cathegories.count then EXIT;
    cath:= SetupCath(setup.cathegories[ix]);
    partSelection.caption:= 'Particles: '+cath.Name;
    if cath.elements.count=0
      then elementsList.items.add('No items found');
    for i:= 0 to cath.elements.count-1 do
    begin
      el:= SetupElement(cath.elements[i]);
      elementsList.items.AddObject(el.name,el);
    end;
  end;
end;

procedure TParticleForm.StepMaxEditChange(Sender: TObject);
begin
end;

procedure TParticleForm.StepMaxEditExit(Sender: TObject);
begin
end;


procedure TParticleForm.PaintParticles(Sender: TObject);
var  minC,maxC,c:double; pcolor,ci:integer;
     x,y:integer; xx,yy,vx,vy,rr:double;
var can:TCanvas; cw,ch,i,r:integer; p:Particle;
     v,rv:Vector3D; transform:^Matrix3D;
     _minX,_maxX,_minY,_maxY:double;
     scale:double;
begin
  if ViewForm<>nil then
  if ViewForm.Visible then
  begin
    can:= ViewForm.particleBitmap.canvas;
    cw:= ViewForm.particleBitmap.width;
    ch:= ViewForm.particleBitmap.height;

    // get ranges
    p:= particle(particleList[0]);
    v:= p.pos;
      _minX:= v[0]; _maxX:= v[0];
      _minY:= v[1]; _maxY:= v[1];
      minC:= p.electric_Charge; maxC:= minC;

    for i:= 1 to particleList.count-1 do
    begin
      p:= Particle(particleList[i]);
      v:= p.pos;
      if v[0]<_minX then _minX:= v[0];
      if v[0]>_maxX then _maxX:= v[0];
      if v[1]<_minY then _minY:= v[1];
      if v[1]>_maxY then _maxY:= v[1];
      if p.electric_charge<minC then minC:= p.electric_charge;
      if p.electric_charge>maxC then maxC:= p.electric_charge;
    end;

    if (not minmaxSet)or(AutoZoom.checked) then
    begin
      minX:= _minX;
      maxX:= _maxX;
      minY:= _minY;
      maxY:= _maxY;
    end;
    minmaxSet:= true;

    // paint it..

    Can.Brush.Color:= clBlack;
    Can.FillRect(0,0,cw,ch);

    for i:= 0 to particleList.count-1 do
    begin
      p:= Particle(particleList[i]);

      v:= p.pos;
      transform:= @ViewForm.transform;
      xx:= transformRow(transform^,v,0);
      yy:= transformRow(transform^,v,1);
      SetVector(rv,v[0]+p.radius,v[1]+p.radius,v[2]+p.radius);
      rr:= xx-transformRow(transform^,rv,0);

      if (maxX-minX)>(maxY-MinY)
        then scale:= 1/(MaxX-MinX)
        else scale:= 1/(maxY-minY);
      vx:= cw*(xx-minX)*scale;
      vy:= ch*(yy-minY)*scale;
      rr:= rr*scale;
      if (vx>0) and (vx<cw) then
      if (vy>0) and (vy<ch) then
      if (rr<cw) then
      begin
        x:= round(vx);
        y:= round(vy);
        r:= round(rr);
        if r=0 then r:= 1;
        if p.color<>0 then
        begin
          pcolor:= p.color;
        end
        else
        begin
          c:= p.electric_charge;
          if c<0
            then
              if minc<0 then c:= c/minc else c:= 0
            else
              if maxc>0 then c:= c/maxc else c:= 0;
          ci:= round(255*c);
          if p.electric_charge<0
            then pcolor:= ci+ (255-ci)*$0100
            else pcolor:= ci*$010000+(255-ci)*$0100;
        end;
        Can.Brush.Color:= pcolor;
        Can.FillRect(x-r,y-r,x+r,y+r);
      end;
    end;
  end;
end;


end.

