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
  SDownloadURL = 'https://automatestudio.org/Updates/';

resourcestring
  SSelectDirectory = 'Select installation destination directory';
  SInstallation = 'Installation';
  SLoadInformation = 'Loading download information: ';
  SDone = 'done';
  SDownloadFiles = 'Downloading and installing files.';
  SFiles = 'files';
  SCreateLink = 'Create .au3proj file link? (y/n)';
  SLoadFile = 'Loading file %s:';
  SInstallationDone = 'Installation completed';
  SAnyKeyExit = 'Press any key to exit';
  SYesShort = 'y';
  SCreateMenuShortcut = 'Create startmenu shortcut? (y/n)';
  SOpenAs = 'Open Automate Studio? (y/n)';

function WriteSize(s: UInt64): String;
begin
  if s<1000 then
    Result:=IntToStr(s)+'B'
  else if s<1000000 then
    Result:=FloatToStrF(s/1000, ffFixed, 10, 2)+'K'
  else if s<1000000000 then
    Result:=FloatToStrF(s/1000000, ffFixed, 10, 2)+'M'
  else if s<1000000000000 then
    Result:=FloatToStrF(s/1000000000, ffFixed, 10, 2)+'G'
  else
    Result:=FloatToStrF(s/1000000000000, ffFixed, 10, 2)+'T'
end;

  procedure TPrintObject.PrintOutput(Sender: TObject;
  const ContentLength, CurrentPos: int64);
  begin
    GotoXY(Length(DownloadName) + 1, WhereY);
    Write(WriteSize(CurrentPos),'...   ');
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
  WriteLn(UTF8Decode(SSelectDirectory));
  fd := TSelectDirectoryDialog.Create(nil);
  try
    fd.InitialDir := GetEnvironmentVariable('PROGRAMFILES(x86)') + PathDelim + 'AutomateStudio';
    fd.FileName := fd.InitialDir;
    fd.Title := SSelectDirectory;
    if not fd.Execute then
      exit;
    if not DirectoryExists(fd.FileName) then
      ForceDirectories(fd.FileName);
    selDir := IncludeTrailingPathDelimiter(fd.FileName);
  finally
    fd.Free;
  end;
  Write(SInstallation,' > ');
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
    po.DownloadName := SLoadInformation;
    Write(SLoadInformation);
    TextColor(White);
    sl.Text := f.Get(SDownloadURL + 'Install.txt');
    TextColor(LightGreen);
    WriteLn(SDone);
    TextColor(White);
    WriteLn(SDownloadFiles);
    i := 0;
    for i := 0 to sl.Count - 1 do
      if sl[i] <> '' then
      begin
        f.RequestHeaders.Clear;
        TextColor(LightCyan);
        po.DownloadName := Format(SLoadFile+' ', [sl[i]]);
        Write(Format(SLoadFile+' ', [sl[i]]));
        TextColor(White);
        ForceDirectories(ExtractFileDir(selDir + sl[i]));
        f.Get(SDownloadURL + StringReplace(ExtractFilePath(sl[i]), '\',
          '/', [rfReplaceAll]) + EncodeURLElement(ExtractFileName(sl[i])),
          selDir + sl[i]);
        TextColor(LightGreen);
        WriteLn(SDone);
      end;
    TextColor(White);
    Write(UTF8Decode(SCreateLink));
    ReadLn(inp);
    if LowerCase(inp) = SYesShort then
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
    Write(UTF8Decode(SCreateMenuShortcut));
    ReadLn(inp);
    if LowerCase(inp) = SYesShort then
    begin
      CreateLink(selDir + 'AU3IDE.exe', GetEnvironmentVariable('HOMEDRIVE') +
        GetEnvironmentVariable('HOMEPATH') +
        PathDelim + 'AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Automate.lnk', 'Automate Studio');
    end;
    TextColor(Yellow);
    WriteLn(SInstallationDone);
    Write(UTF8Decode(SOpenAS));
    ReadLn(inp);
    if LowerCase(inp) = SYesShort then
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
