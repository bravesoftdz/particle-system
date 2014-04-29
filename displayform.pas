unit displayform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lmessages, types, vectors;

type

  { TViewForm }

  TViewForm = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
      procedure PaintBox1Paint(Sender: TObject);
      procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    smaxX,smaxY,sminX,sminY:double;
    oldTransform:Matrix3d;
  public
    { public declarations }
      inval:boolean;
      draggingIt:boolean;
      turningIt:boolean;
      dragX,dragY,toX,toY:integer;
      repaintEvent:TNotifyEvent;
      particleBitmap:TBitmap;
      transform:Matrix3d;
      procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;

  end;

var
  ViewForm: TViewForm;

implementation

{$R *.lfm}

uses partform;

procedure TViewForm.FormCreate(Sender: TObject);
begin
  particleBitmap:= TBitmap.create;
  particleBitmap.Width:= PaintBox1.Width;
  particleBitmap.height:= paintBox1.height;
  particleBitmap.PixelFormat:= pf32bit;
  transform[0,0]:= 1;
  transform[1,1]:= 1;
  transform[2,2]:= 1;
end;

procedure TViewForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //start drag
  if not draggingIt then
  begin
    dragX:= x;
    dragY:= y;
    sminx:= ParticleForm.minX;
    sminy:= ParticleForm.minY;
    smaxX:= ParticleForm.maxX;
    smaxY:= ParticleForm.maxY;
    if Button=mbRight then
    begin
      turningIt:= true;
      oldTransform:= transform;
    end;
  end;
  draggingIt:= true;
end;

procedure TViewForm.FormMouseLeave(Sender: TObject);
begin
  // stop drag
  draggingIt:= false;
  turningIt:= false;
end;

procedure TViewForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var cx,cy,rx,ry,sx,sy:double; temp:Matrix3D;
begin
  //move
  if draggingIt then
  begin
    if turningIt then
    begin
      rx:= -(dragX-x)/paintBox1.width;
      ry:= (dragY-y)/paintBox1.height;

      RotateMatrix(oldTransform,rx,1,temp);
      RotateMatrix(temp,ry,0,transform);

      toX:= x;
      toY:= y;
      inval:= true;
    end
    else
    begin
      cx:= (sminX+smaxX)/2;
      cy:= (sminY+smaxY)/2;
      rx:= (sMaxX-sminX)/2;
      ry:= (sMaxY-sminY)/2;
      ParticleForm.minx:= cx- rx +(dragX-x)*rx*2/paintBox1.width;
      ParticleForm.maxx:= cx+ rx +(dragX-x)*rx*2/paintBox1.width;
      ParticleForm.MinY:= cy-ry +(dragY-y)*ry*2/paintBox1.height;
      ParticleForm.MaxY:= cy+ry +(dragY-y)*ry*2/paintBox1.height;
      toX:= x;
      toY:= y;
      inval:= true;
    end;
  end;
end;

procedure TViewForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // stop drag
  draggingIt:= false;
  turningIt:= false;
end;


procedure TViewForm.FormResize(Sender: TObject);
begin
  if particleBitmap<>nil then
  begin
    particleBitmap.Width:= PaintBox1.Width;
    particleBitmap.height:= paintBox1.height;
    PaintBox1.invalidate;

  end;
end;

procedure TViewForm.PaintBox1MouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
  var minx,maxx,miny,maxy,cx,cy,rx,ry:double;
begin
  //zoom out
  minx:= ParticleForm.minX;
  miny:= ParticleForm.minY;
  maxX:= ParticleForm.maxX;
  maxY:= ParticleForm.maxY;
  cx:= (minX+maxX)/2;
  cy:= (minY+maxY)/2;
  rx:= (MaxX-minX)*1.1/2;
  ry:= (MaxY-minY)*1.1/2;
  ParticleForm.minx:= cx- rx;
  ParticleForm.maxx:= cx+ rx;
  ParticleForm.MinY:= cy-ry;
  ParticleForm.MaxY:= cy+ry;
  inval:= true;
end;

procedure TViewForm.PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
  var minx,maxx,miny,maxy,cx,cy,rx,ry:double;
begin
  //zoom in
  minx:= ParticleForm.minX;
  miny:= ParticleForm.minY;
  maxX:= ParticleForm.maxX;
  maxY:= ParticleForm.maxY;
  cx:= (minX+maxX)/2;
  cy:= (minY+maxY)/2;
  rx:= (MaxX-minX)/1.1/2;
  ry:= (MaxY-minY)/1.1/2;
  ParticleForm.minx:= cx- rx;
  ParticleForm.maxx:= cx+ rx;
  ParticleForm.MinY:= cy-ry;
  ParticleForm.MaxY:= cy+ry;
  inval:= true;
end;

procedure TViewForm.PaintBox1Paint(Sender: TObject);
begin
  if particleBitmap<>nil then
  begin
    PaintBox1.Canvas.Draw(0,0,particlebitmap);
    PaintBox1.Canvas.Pen.color:= clRed;
    if draggingIt then
      PaintBox1.Canvas.Line(DragX,DragY,toX,toY);
  end
  else
  begin
    PaintBox1.Canvas.FillRect(0,0,PaintBox1.width,PaintBox1.height);
  end;
end;

procedure TViewForm.Timer1Timer(Sender: TObject);
begin
  if inval then
  begin
    inval:= false;
    repaintEvent(nil);
    PaintBox1.invalidate;
  end;
end;

procedure TViewForm.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  // no erase needed
end;



end.

