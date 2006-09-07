unit MapFiles;

interface

uses
  SysUtils, Classes, ScUtils, FunLabyUtils, FilesUtils;

type
  {*
    Représente un fichier carte de type 'application/flm'
  *}
  TFLMMapFile = class(TMapFile)
  protected
    procedure SaveFile; override;
  public
    constructor Create(AMasterFile : TMasterFile; const AFileName : TFileName;
      const AMIMEType : string; const AMapID : TComponentID); override;
  end;

implementation

//////////////////////////
/// Classe TFLMMapFile ///
//////////////////////////

const {don't localize}
  FLMMIMEType = 'application/flm';
  FLMFormatCode : LongInt = $6D6C662E; // Correspond à '.flm'
  FLMVersion = 1;

{*
  Crée une instance de TMapFile
  @param AMasterFile   Fichier maître
  @param AFileName     Nom du fichier
  @param AMIMEType     Type MIME du fichier
  @param AMapID        ID de la carte
*}
constructor TFLMMapFile.Create(AMasterFile : TMasterFile;
  const AFileName : TFileName; const AMIMEType : string;
  const AMapID : TComponentID);
var Stream : TStream;
    I, Count, X, Y, Z, ReadIndex : integer;
    Dimensions : T3DPoint;
    Palette : array of TScrew;
    Read : array[0..3] of integer;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    // Contrôle de format
    Stream.ReadBuffer(I, 4);
    if I <> FLMFormatCode then
      EFileError.Create(sInvalidFileFormat);

    // Contrôle de version de format
    Stream.ReadBuffer(I, 4);
    if I > FLMVersion then
      EFileError.CreateFmt(sVersionTooHigh, [IntToStr(I)]);

    // Lecture des dimensions
    Stream.ReadBuffer(Dimensions, sizeof(T3DPoint));

    // On peut enfin appeler le constructeur hérité
    inherited Create(AMasterFile, AFileName, AMIMEType, AMapID, Dimensions);

    // Lecture de la palette de cases
    Stream.ReadBuffer(Count, 4);
    SetLength(Palette, Count);
    for I := 0 to Count-1 do
      Palette[I] := Master.Screw[ReadStrFromStream(Stream)];

    // Lecture de la carte
    I := 0;
    ReadIndex := -1;
    for Z := 0 to Dimensions.Z-1 do
      for Y := 0 to Dimensions.Y-1 do
        for X := 0 to Dimensions.X-1 do
    begin
      // S'il n'y a plus de cases pré-lues, lire de nouvelles cases
      if ReadIndex < 0 then
      begin
        Stream.ReadBuffer(I, 2);
        if Count <= 16 then
        begin
          Read[0] := I shr 12;
          Read[1] := (I shr 8) and $0F;
          Read[2] := (I shr 4) and $0F;
          Read[3] := I and $0F;
          ReadIndex := 3;
        end else
        if Count <= 256 then
        begin
          Read[0] := I shr 8;
          Read[1] := I and $FF;
          ReadIndex := 1;
        end else
        begin
          Read[0] := I;
          ReadIndex := 0;
        end;
      end;

      // Ajouter une case pré-lue dans la carte
      Map[Point3D(X, Y, Z)] := Palette[Read[ReadIndex]];

      // Passer à la case pré-lue suivante
      dec(ReadIndex);
    end;
  finally
    Free;
  end;
end;

{*
  Enregistre le fichier
*}
procedure TFLMMapFile.SaveFile;
begin
end;

initialization
  TMasterFile.RegisterMapFileClass(FLMMIMEType, TFLMMapFile);
end.

