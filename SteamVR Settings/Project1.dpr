program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
