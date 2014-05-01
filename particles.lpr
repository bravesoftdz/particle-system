program particles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, partform, displayform, vectors, particle, partsystem, setupreader,
  dbflaz, sdflaz, memdslaz, tachartlazaruspkg
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TParticleForm, ParticleForm);
  Application.CreateForm(TViewForm, ViewForm);
  Application.Run;
end.

