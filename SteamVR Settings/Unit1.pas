unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, XPMan, Registry;

type
  TMain = class(TForm)
    DisplayTB: TTrackBar;
    ChsDisplay: TLabel;
    RndResLbl: TLabel;
    RndWidthEdt: TEdit;
    XLbl: TLabel;
    RndHeightEdt: TEdit;
    FreeTrackRB: TRadioButton;
    UDPRB: TRadioButton;
    ChsDriverLbl: TLabel;
    ApplyBtn: TButton;
    CancelBtn: TButton;
    AboutBtn: TButton;
    XPManifest: TXPManifest;
    DisplayLbl: TLabel;
    DbgMdCb: TCheckBox;
    DbgMdLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure DisplayTBChange(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

{$R *.dfm}

procedure TMain.FormCreate(Sender: TObject);
begin
  Application.Title:=Caption;

  SetWindowLong(RndWidthEdt.Handle, GWL_STYLE, GetWindowLong(RndWidthEdt.Handle, GWL_STYLE) or ES_NUMBER);
  SetWindowLong(RndHeightEdt.Handle, GWL_STYLE, GetWindowLong(RndHeightEdt.Handle, GWL_STYLE) or ES_NUMBER);

  DisplayTB.Max:=Screen.MonitorCount - 1;
  RndWidthEdt.Text:=IntToStr(Screen.Monitors[0].Width);
  RndHeightEdt.Text:=IntToStr(Screen.Monitors[0].Height);

  DbgMdLbl.Caption:=DbgMdLbl.Caption + #13#10 + 'Windowed borderless fullscreen' + #13#10 + 'with lock to 30 FPS';
end;

procedure TMain.AboutBtnClick(Sender: TObject);
begin
  Application.MessageBox('OpenVR OpenTrack' + #13#10 +
  'https://github.com/r57zone/OpenVR-OpenTrack' + #13#10 +
  'r57zone@gmail.com', PChar(Caption), MB_ICONINFORMATION);
end;

procedure TMain.DisplayTBChange(Sender: TObject);
begin
  RndWidthEdt.Text:=IntToStr(Screen.Monitors[DisplayTB.Position].Width);
  RndHeightEdt.Text:=IntToStr(Screen.Monitors[DisplayTB.Position].Height);
  DisplayLbl.Caption:=IntToStr(DisplayTB.Position + 1);
end;

procedure TMain.ApplyBtnClick(Sender: TObject);
var
  Reg: TRegistry;
  SteamPath: string;
  Config: TStringList;
  Error: boolean;
begin
  Error:=false;
  Reg:=TRegistry.Create;
  Reg.RootKey:=HKEY_CURRENT_USER;
  if (Reg.OpenKey('\Software\Valve\Steam', false)) then
    SteamPath:=StringReplace(Reg.ReadString('SteamPath'), '/', '\', [rfReplaceAll]);
  Reg.CloseKey;
  Reg.Free;

  if DirectoryExists(SteamPath) then begin

    //Обновлнение и копирование конфига OpenVR
    if FileExists(ExtractFilePath(ParamStr(0)) + 'OpenVR\steamvr.vrsettings') then begin
      Config:=TStringList.Create;
      Config.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'OpenVR\steamvr.vrsettings');
      Config.Text:=StringReplace(Config.Text, '<RENDERWIDTH>', RndWidthEdt.Text, [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<RENDERHEIGHT>', RndHeightEdt.Text, [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWWIDTH>', IntToStr(Screen.Monitors[DisplayTB.Position].Width), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWHEIGHT>', IntToStr(Screen.Monitors[DisplayTB.Position].Height), [rfReplaceAll]);

      Config.Text:=StringReplace(Config.Text, '<WINDOWX>', IntToStr(Screen.Monitors[DisplayTB.Position].Left), [rfReplaceAll]);
      Config.Text:=StringReplace(Config.Text, '<WINDOWY>', IntToStr(Screen.Monitors[DisplayTB.Position].Top), [rfReplaceAll]);

      if DbgMdCb.Checked then
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'true', [rfReplaceAll])
      else
        Config.Text:=StringReplace(Config.Text, '<DEBUGMODE>', 'false', [rfReplaceAll]);

      Config.SaveToFile(SteamPath + '\config\steamvr.vrsettings');

      Config.Free;
    end else begin
      Application.MessageBox('File "steamvr.vrsettings" not found.', PChar(Caption), MB_ICONERROR);
      Error:=true;
    end;

    //Копирование драйверов
    if UDPRB.Checked then begin  //UDP

      if not ((CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverUDP32.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win32\driver_null.dll'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverUDP64.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win64\driver_null.dll'), false))) then begin
        Application.MessageBox('Error copy driver files. Please close Steam and SteamVR.', PChar(Caption), MB_ICONERROR);
        Error:=true;
      end;

    end else begin //FreeTrack

      if not ((CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverFreeTrack32.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win32\driver_null.dll'), false)) and
      (CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'OpenVR\DriverFreeTrack64.dll'), PChar(SteamPath + '\steamapps\common\SteamVR\drivers\null\bin\win64\driver_null.dll'), false))) then begin
        Application.MessageBox('Error copy driver files. Please close Steam and SteamVR.', PChar(Caption), MB_ICONERROR);
        Error:=true;
      end;
    end;

    if Error = false then
      Application.MessageBox('Done', PChar(Caption), MB_ICONINFORMATION);

  end else
    ShowMessage('Steam not found. Please install Steam and SteamVR');
  
  if Error = false then
    Close;
end;

procedure TMain.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

end.
