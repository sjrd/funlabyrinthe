unit LibraryDatabase;

interface

uses
  SysUtils, Classes, Contnrs, GitTools;

type
  TLibraryFileStatus = (fsUpToDate, fsRemoteUpdated, fsRemoteAdded,
    fsRemoteDeleted, fsLocalUpdated, fsLocalAdded);

  {*
    Informations locales sur un fichier de la bibliothèque
    @author sjrd
    @version 5.2
  *}
  TLibraryFile = class(TObject)
  private
    FPath: TFileName;   /// Chemin de ce fichier dans la Library
    FLocalHash: THash;  /// Hash local
    FRemoteHash: THash; /// Hash distant

    FOldHashes: TStrings; /// Anciens hashes distants

    function GetIsDeleted: Boolean;
    function GetStatus: TLibraryFileStatus;
  public
    constructor Create(const APath: TFileName);
    destructor Destroy; override;

    property Path: TFileName read FPath;
    property LocalHash: THash read FLocalHash write FLocalHash;
    property RemoteHash: THash read FRemoteHash write FRemoteHash;

    property OldHashes: TStrings read FOldHashes;

    property IsDeleted: Boolean read GetIsDeleted;
    property Status: TLibraryFileStatus read GetStatus;
  end;

  {*
    Liste des fichiers de la bibliothèque
    @author sjrd
    @version 5.2
  *}
  TLibraryFileList = class(TObjectList)
  private
    function GetItems(Index: Integer): TLibraryFile;
  public
    function GetFile(const APath: TFileName): TLibraryFile;

    property Items[Index: Integer]: TLibraryFile read GetItems; default;
  end;

implementation

const {don't localize}
  StrDeletedHash = THash('<deleted>');

{--------------------}
{ TLibraryFile class }
{--------------------}

{*
  Crée une instance de TLibraryFile
  @param APath   Chemin du fichier dans la bibliothèque
*}
constructor TLibraryFile.Create(const APath: TFileName);
begin
  inherited Create;

  FPath := APath;
  FOldHashes := TStringList.Create;
end;

{*
  [@inheritDoc]
*}
destructor TLibraryFile.Destroy;
begin
  FOldHashes.Free;

  inherited;
end;

{*
  True ssi le fichier a été supprimé sur Internet
*}
function TLibraryFile.GetIsDeleted: Boolean;
begin
  Result := FRemoteHash = StrDeletedHash;
end;

{*
  Statut du fichier
*}
function TLibraryFile.GetStatus: TLibraryFileStatus;
begin
  if LocalHash = '' then
    Result := fsRemoteAdded
  else if RemoteHash = '' then
    Result := fsLocalAdded
  else if LocalHash = RemoteHash then
    Result := fsUpToDate
  else if OldHashes.IndexOf(LocalHash) >= 0 then
  begin
    if IsDeleted then
      Result := fsRemoteDeleted
    else
      Result := fsRemoteUpdated;
  end else
    Result := fsLocalUpdated;
end;

{------------------------}
{ TLibraryFileList class }
{------------------------}

{*
  Tableau zero-based des fichiers de la bibliothèque
  @param Index   Index compris entre 0 inclus et Count exclu
  @return Le fichier à l'index spécifié
*}
function TLibraryFileList.GetItems(Index: Integer): TLibraryFile;
begin
  Result := (inherited Items[Index]) as TLibraryFile;
end;

{*
  Cherche un fichier de la bibliothèque d'après son chemin
  @param APath   Chemin dans la bibliothèque
  @return Le fichier correspondant
*}
function TLibraryFileList.GetFile(const APath: TFileName): TLibraryFile;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := Items[I];
    if AnsiSameText(Result.Path, APath) then
      Exit;
  end;

  Result := TLibraryFile.Create(APath);
  Add(Result);
end;

end.
