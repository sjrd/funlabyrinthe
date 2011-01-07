{*
  Interface avec TortoiseGIT
  @author sjrd
  @version 5.0
*}
unit TortoiseGIT;

interface

uses
  Windows, SysUtils, Classes;

type
  {*
    Interface avec TortoiseGIT
    @author sjrd
    @version 5.2
  *}
  TTortoiseGIT = class(TObject)
  private
    FProcPath: TFileName; /// Path vers TortoiseProc.exe
    FEnabled: Boolean;    /// True si TortoiseProc.exe a été trouvé

    FPath: TFileName; /// Path vers le repository

    procedure FindProcPath;

    procedure ExecuteCommand(const Command: string;
      const Params: array of string);
  public
    constructor Create(const APath: TFileName);

    procedure ShowLog;

    property Enabled: Boolean read FEnabled;

    property Path: TFileName read FPath;
  end;

implementation

uses
  Registry, FunLabyRepoConsts;

{--------------------}
{ TTortoiseGIT class }
{--------------------}

{*
  Crée une interface avec TortoiseGIT
  @param APath   Path vers le repository
*}
constructor TTortoiseGIT.Create(const APath: TFileName);
begin
  inherited Create;

  FindProcPath;
  Assert(Enabled);

  FPath := APath;
end;

{*
  Trouve le path jusque TortoiseProc.exe
*}
procedure TTortoiseGIT.FindProcPath;
const {don't localize}
  TortoiseGITKey = '\Software\TortoiseGIT';
  ProcPathName = 'ProcPath';
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    // First try the 64 bit key, then fall back on 32 bit key
    Registry.Access := KEY_READ or KEY_WOW64_64KEY;
    if not Registry.OpenKey(TortoiseGITKey, False) then
    begin
      Registry.Access := KEY_READ or KEY_WOW64_32KEY;
      if not Registry.OpenKey(TortoiseGITKey, False) then
        Exit;
    end;

    FProcPath := Registry.ReadString(ProcPathName);
    FEnabled := FProcPath <> '';
  finally
    Registry.Free;
  end;
end;

{*
  Execute une commande de TortoiseGIT
  @param Command   Commande à exécuter
  @param Params    Paires param/value des paramètres
*}
procedure TTortoiseGIT.ExecuteCommand(const Command: string;
  const Params: array of string);
var
  CommandLine, Param, Value: string;
  I: Integer;
  ImplicitPath: Boolean;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CreateProcessResult: Boolean;
begin
  if not Enabled then
    raise EAbort.Create(SCannotStartTortoiseGIT);

  // Build up command line
  CommandLine := Format('"%s" /command:%s', [FProcPath, Command]);
  ImplicitPath := True;

  for I := 0 to Length(Params) div 2 - 1 do
  begin
    Param := Params[2*I];
    Value := Params[2*I+1];
    CommandLine := CommandLine + Format(' /%s:"%s"', [Param, Value]);

    if Param = 'path' then
      ImplicitPath := False;
  end;

  if ImplicitPath then
    CommandLine := CommandLine + Format(' /path:"%s"', [FPath]);

  // Create the TortoiseProc process
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  FillChar(ProcessInfo, SizeOf(ProcessInfo), 0);
  CreateProcessResult := CreateProcess(PChar(FProcPath), PChar(CommandLine),
    nil, nil, False, 0, nil, PChar(FPath), StartupInfo, ProcessInfo);

  if not CreateProcessResult then
    raise EAbort.Create(SCannotStartTortoiseGIT);
end;

{*
  Affiche le log du repository
*}
procedure TTortoiseGIT.ShowLog;
begin
  ExecuteCommand('log', []);
end;

end.

