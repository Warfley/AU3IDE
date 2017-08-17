unit TLStrings;

{$mode objfpc}{$H+}

interface

resourcestring
  SDoneCompileing = '%s compiled';
  SDoneExec = 'Excecution finished';
  SErrorCompiling = 'Error: %s at "%s" <%d:%d>';

  SStillRunningTitle = 'Excecution has not finished';
  SStillRunningText = 'Excecution of the compiler/project has not finished. Terminate it now?';
  SStillRunningKeyword = 'Terminate';

  SNewUpdateTitle = 'New version available';
  SNewUpdateText = 'A new version is available.'#10#13'Update now?';
  SNewUpdateKeyword = 'Update';
  SNoNewUpdate = 'No new version available.';

  SSearchForUpdatesTitle = 'Search for updates?';
  SSearchForUpdatesText = 'Search for updates on startup?';
  SSearchForUpdatesKeyword = 'Updates';

  SSaveProjectTitle = 'Save project?';
  SSaveProjectText = 'The project got changed.'#10#13'Save changes?';
  SSaveProjectKeyword = 'Save';

  SSaveFileTitle = 'Save file?';
  SSaveFileText = 'The file got changed.'#10#13'Save changes?';
  SSaveFileKeyword = 'Save';

  SRunBtnHint = 'Run %s (%s)';

  SProjectInspector = 'Project inspector';
  SLeft = 'Left';
  SRight = 'Right';

  SAutoIt = 'AutoIt';
  SForm = 'form';
  SUnit = 'source file';
  SProgrammFile = 'main source';

  SSelectProjectTemplate = 'Select a project template';
  SAllowedSymbols = 'Names must only consist of alphanumerical chars and _';
  SErrorCreateProjectDir = 'An error occured creating project directories';

  SDirNotEmptyTitle = 'Directory not empty';
  SDirNotEmptyText = 'The chosen directory is not empty, the files might be overwritten.'#10#13'Continue?';
  SDirNotEmptyKeyword = 'Overwrite files';

  SComp0 = 'None';
  SComp1 = 'Weak';
  SComp2 = 'Normal';
  SComp3 = 'Strong';
  SComp4 = 'Max';

  SFileNotProjDirTitle = 'File located outside the project directory';
  SFileNotProjDirText = 'The selected file is not located in the project directory.'#10#13'Do you want to create a copy in the project directory?';
  SFileNotProjDirKeyword = 'Copy to Project';

  SDeleteFileTitle = 'Delete file?';
  SDeleteFileText = 'Delete the selected file?';
  SDeleteFileKeyword = 'Continue';

  SProject = 'Project';
  SOpend = 'Opend';
  SFile = 'File';

  SNoMatchFound = 'No match found';

  SEOFReachedTitle = 'End of file reached';
  SEOFReachedText = 'End of file reached, continue at the first line?';
  SEOFReachedKeyword = 'EOF reached';

  SCodeFormatterTitle = 'Format code?';
  SCodeFormatterText = 'Start code formatting?'#10#13'(This could fuck up your code)';
  SCodeFormatterKeyword = 'Format';

  SCodeExplorer = 'Code explorer';

  SNew = 'New';
  SNone = 'None';

  SInclude='Include';
  SVariables = 'Variables';
  SFunctions = 'Functions';

  SDefault = 'Default';

  SLanguageText = 'Language changes will only apply after restarting the application';
  SLanguageTitle= 'Language Changed';
  SLanguageKeyword = 'Language';

implementation

end.

