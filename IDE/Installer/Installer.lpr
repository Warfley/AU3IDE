program Installer;

{$mode objfpc}{$H+}

uses
  ShlObj,
  ActiveX,
  ComObj,
  Windows,
  SysUtils,
  Classes,
  fphttpclient,
  crt,
  Dialogs,
  LCLIntf,
  LCLProc,
  LCLType,
  LCL,
  process,
  uFileAssociation { you can add units after this };

type
  TPrintObject = class
  public
    DownloadName: string;
    procedure PrintOutput(Sender: TObject; const ContentLength, CurrentPos: int64);
  end;

const
  SDownloadURL = 'http://kehrein.org/AS/Install/';

  procedure TPrintObject.PrintOutput(Sender: TObject;
  const ContentLength, CurrentPos: int64);
  begin
    GotoXY(Length(DownloadName) + 1, WhereY);
    ClrEol;
    Write(CurrentPos);
    Write('b ...');
  end;


function CreateLink(const AFilename, ALNKFilename, ADescription: string) : Boolean;
var
  psl : IShellLink;
  ppf : IPersistFile;
  wsz : PWideChar;
begin
  result:=false;
  if SUCCEEDED(CoCreateInstance(CLSID_ShellLink, nil,
  CLSCTX_inPROC_SERVER, IID_IShellLinkA, psl)) then
  begin
    psl.SetPath(PChar(AFilename));
    psl.SetDescription(PChar(ADescription));
    psl.SetWorkingDirectory(PChar(ExtractFilePath(AFilename)));
    if SUCCEEDED(psl.QueryInterface(IPersistFile, ppf)) then
    begin
      GetMem(wsz, MAX_PATH*2);
      try
        MultiByteToWideChar(CP_ACP, 0, PChar(ALNKFilename), -1, wsz, MAX_PATH);
        ppf.Save(wsz, true);
        result:=true;
      finally
        FreeMem(wsz, MAX_PATH*2);
      end;
    end;
  end;
end;


var
  fd: TSelectDirectoryDialog;
  sl: TStringList;
  f: TFPHTTPClient;
  po: TPrintObject;
  p: TProcess;
  selDir: string;
  i: integer;
  inp: string;
  a: TFileAssociation;

{$R *.res}

begin
  TextColor(White);
  WriteLn(UTF8Decode('Bitte das Zielverzeichnis wählen'));
  fd := TSelectDirectoryDialog.Create(nil);
  try
    fd.InitialDir := GetEnvironmentVariable('PROGRAMFILES(x86)') + PathDelim + 'AutomateStudio';
    fd.FileName := fd.InitialDir;
    fd.Title := 'Installationsverzeichnis wählen';
    if not fd.Execute then
      exit;
    if not DirectoryExists(fd.FileName) then
      ForceDirectories(fd.FileName);
    selDir := IncludeTrailingPathDelimiter(fd.FileName);
  finally
    fd.Free;
  end;
  Write('Installation > ');
  TextColor(LightCyan);
  WriteLn(selDir);
  TextColor(White);
  // Installation
  sl := TStringList.Create;
  po := TPrintObject.Create;
  f := TFPHTTPClient.Create(nil);
  try
    f.OnDataReceived := @po.PrintOutput;
    TextColor(LightCyan);
    po.DownloadName := 'Installationsinformationen werden geladen: ';
    Write('Installationsinformationen werden geladen: ');
    TextColor(White);
    sl.Text := f.Get(SDownloadURL + 'Install.txt');
    TextColor(LightGreen);
    WriteLn('Fertig');
    TextColor(White);
    WriteLn('Daten werden heruntergeladen und installiert.');
    i := 0;
    for i := 0 to sl.Count - 1 do
      if sl[i] <> '' then
      begin
        f.RequestHeaders.Clear;
        TextColor(LightCyan);
        po.DownloadName := Format('Datei %s wird geladen: ', [sl[i]]);
        Write(Format('Datei %s wird geladen: ', [sl[i]]));
        TextColor(White);
        ForceDirectories(ExtractFileDir(selDir + sl[i]));
        f.Get(SDownloadURL + StringReplace(ExtractFilePath(sl[i]), '\',
          '/', [rfReplaceAll]) + EncodeURLElement(ExtractFileName(sl[i])),
          selDir + sl[i]);
        TextColor(LightGreen);
        WriteLn('Fertig');
      end;
    TextColor(White);
    Write(UTF8Decode('.au3proj Dateien verknüpfen? (y/n)'));
    ReadLn(inp);
    if LowerCase(inp) = 'y' then
    begin
      a := TFileAssociation.Create(nil);
      try
        a.ApplicationDescription := 'Automate Studio: Autoit IDE';
        a.Extension := '.au3proj';
        a.ExtensionName := 'Automate Studio Project';
        a.Action := '"' + selDir + 'AU3IDE.exe" "%1"';
        a.ActionName := 'Open';
        a.Execute;
      finally
        a.Free;
      end;
    end;
    Write(UTF8Decode('Startmenü Verknüpfung erstellen? (y/n)'));
    ReadLn(inp);
    if LowerCase(inp) = 'y' then
    begin
      CreateLink(selDir + 'AU3IDE.exe', GetEnvironmentVariable('HOMEDRIVE') +
        GetEnvironmentVariable('HOMEPATH') +
        PathDelim + 'AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Automate.lnk', 'Automate Studio');
    end;
    TextColor(Yellow);
    WriteLn('Installation abgeschlossen');
    Write(UTF8Decode('Automate Studio öffnen? (y/n)'));
    ReadLn(inp);
    if LowerCase(inp) = 'y' then
    begin
      p := TProcess.Create(nil);
      try
        p.Executable := selDir + 'AU3IDE.exe';
        p.Execute;
      finally
        p.Free;
      end;
    end;
  finally
    sl.Free;
    po.Free;
    f.Free;
  end;
end.
