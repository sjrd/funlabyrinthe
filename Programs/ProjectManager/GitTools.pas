unit GitTools;

interface

uses
  SysUtils, Classes, StrUtils, IdHashSHA1, FilesUtils, ScStrUtils;

type
  /// Hash de Git
  THash = type string;

  {*
    Motifs de .gitignore
    @author sjrd
    @version 5.2
  *}
  TGitIgnorePatterns = class(TStringList)
  private
    FRepoDir: TFileName; /// Top-level repo dir
    FSubDir: TFileName;  /// Subdirectory

    procedure LoadPatternFile;

    function MatchesLine(const FileName: TFileName;
      const Line: string): Boolean;
  public
    constructor Create(const ARepoDir: TFileName); overload;
    constructor Create(Parent: TGitIgnorePatterns;
      const ASubDir: TFileName); overload;

    function IsIgnored(const FileName: TFileName): Boolean;

    property RepoDir: TFileName read FRepoDir;
    property SubDir: TFileName read FSubDir;
  end;

function GitHashBlob(const FileName: TFileName): THash;

implementation

{*
  Calcule le hash d'un blob pour Git
  @param FileName   Nom du fichier blob
  @return Hash du fichier pour Git
*}
function GitHashBlob(const FileName: TFileName): THash;
var
  FileStream, HashedStream: TStream;
  SHA1: TIdHashSHA1;
  MetaData: AnsiString;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  HashedStream := TMemoryStream.Create;
  SHA1 := TIdHashSHA1.Create;
  try
    MetaData := 'blob ' + AnsiString(IntToStr(FileStream.Size));
    HashedStream.WriteBuffer(MetaData[1], Length(MetaData)+1);
    HashedStream.CopyFrom(FileStream, 0);
    HashedStream.Position := 0;
    Result := AnsiLowerCase(SHA1.HashStreamAsHex(HashedStream));
  finally
    FileStream.Free;
    HashedStream.Free;
    SHA1.Free;
  end;
end;

{--------------------------}
{ TGitIgnorePatterns class }
{--------------------------}

{*
  Crée une instance de TGitIgnorePatterns
  @param ARepoDir   Dossier dans lequel se trouve le repo
*}
constructor TGitIgnorePatterns.Create(const ARepoDir: TFileName);
begin
  inherited Create;

  FRepoDir := ARepoDir;

  LoadPatternFile;
end;

{*
  Crée une instance de TGitIgnorePatterns
  @param Parent    Patterns du dossier parent
  @param ASubDir   Sous-dossier
*}
constructor TGitIgnorePatterns.Create(Parent: TGitIgnorePatterns;
  const ASubDir: TFileName);
begin
  inherited Create;

  FRepoDir := Parent.RepoDir;
  FSubDir := JoinPath([Parent.SubDir, ASubDir]);

  LoadPatternFile;
  AddStrings(Parent);
end;

{*
  Charge le fichier .gitignore
*}
procedure TGitIgnorePatterns.LoadPatternFile;
var
  FileName: TFileName;
begin
  FileName := JoinPath([RepoDir, SubDir, '.gitignore']);
  if not FileExists(FileName) then
    Exit;

  LoadFromFile(FileName);
end;

{*
  Teste si un nom de fichier correspond à un motif du .gitignore
  @param FileName   Nom du fichier
  @param Line       Motif du .gitignore
  @return True ssi le nom de fichier correspond au motif
*}
function TGitIgnorePatterns.MatchesLine(const FileName: TFileName;
  const Line: string): Boolean;
var
  Name, StartStr, EndStr: string;
begin
  Name := ExtractFileName(FileName);
  Result := AnsiMatchText(Name, Line);

  if not Result and SplitToken(Line, '*', StartStr, EndStr) then
    Result := AnsiStartsText(StartStr, Name) and AnsiEndsText(EndStr, Name);
end;

{*
  Teste si un fichier doit être ignoré
  @param FileName   Nom du fichier
  @return True ssi le fichier doit être ignoré
*}
function TGitIgnorePatterns.IsIgnored(const FileName: TFileName): Boolean;
var
  I: Integer;
  Line: string;
begin
  for I := 0 to Count-1 do
  begin
    Line := Strings[I];

    if (Line = '') or (Line[1] = '#') then
      Continue;

    if MatchesLine(FileName, Line) then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

end.
