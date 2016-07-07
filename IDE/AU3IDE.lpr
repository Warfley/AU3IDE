program AU3IDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, runtimetypeinfocontrols, IDEMainForm, Project,
  IDEStartupScreen, ProjectInspector, au3Types, Editor, FormEditor,
  EditorManagerFrame, au3FileInfo, FormEditComponents, au3Compiler,
  CompilerOptions, EditorOptions, FormEditorOptions, SampeProjectView,
  AboutWindow, ProjectConfForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='AU3IDE';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStartupScreen, StartupScreen);
  Application.CreateForm(TCompilerOptionsForm, CompilerOptionsForm);
  Application.CreateForm(TEditorConf, EditorConf);
  Application.CreateForm(TFormEditorOptionsForm, FormEditorOptionsForm);
  Application.CreateForm(TSampleForm, SampleForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TProjectSettings, ProjectSettings);
  Application.Run;
end.

