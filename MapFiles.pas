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
    I, Count, Value, ReadIndex : integer;
    Dimensions : T3DPoint;
    Palette : array of TScrew;
    Read : array[0..3] of integer;

  procedure ReadFurther;
  begin
    // S'il n'y a plus de cases pré-lues, lire de nouvelles cases
    if ReadIndex < 0 then
    begin
      Stream.ReadBuffer(Value, 2);
      if Count <= 16 then
      begin
        // Attention ici : à l'intérieur d'un octet, on est en Little Endian
        Read[0] := (Value shr 8) and $0F;
        Read[1] := Value shr 12;
        Read[2] := Value and $0F;
        Read[3] := (Value shr 4) and $0F;
        ReadIndex := 3;
      end else
      if Count <= 256 then
      begin
        Read[0] := Value shr 8;
        Read[1] := Value and $FF;
        ReadIndex := 1;
      end else
      begin
        Read[0] := Value;
        ReadIndex := 0;
      end;
    end;
  end;

begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    // Contrôle de format
    Stream.ReadBuffer(Value, 4);
    if Value <> FLMFormatCode then
      EFileError.Create(sInvalidFileFormat);

    // Contrôle de version de format
    Stream.ReadBuffer(Value, 4);
    if Value > FLMVersion then
      EFileError.CreateFmt(sVersionTooHigh, [IntToStr(Value)]);

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
    ReadIndex := -1;
    Value := 0;
    for I := 0 to Map.LinearMapCount-1 do
    begin
      ReadFurther;

      // Ajouter une case pré-lue dans la carte
      Map.LinearMap[I] := Palette[Read[ReadIndex]];

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
var I, Value, Count, WritingIndex : integer;
    Stream : TStream;
    Dimensions : T3DPoint;
    Writing : array[0..3] of integer;

  procedure WriteFurther;
  begin
    // Si on ne peut plus prévoir de case à écrire, les écrire effectivement
    if WritingIndex < 0 then
    begin
      if Count <= 16 then
      begin
        // Attention ici : à l'intérieur d'un octet, on est en Little Endian
        Value := Writing[3] shl 8 + Writing[2] shl 12 +
          Writing[1] + Writing[0] shl 4;
        WritingIndex := 3;
      end else
      if Count <= 256 then
      begin
        Value := Writing[1] shl 8 + Writing[0];
        WritingIndex := 1;
      end else
      begin
        Value := Writing[0];
        WritingIndex := 0;
      end;

      Stream.WriteBuffer(Value, 2);
    end;
  end;

begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    // Indication de format
    Value := FLMFormatCode;
    Stream.WriteBuffer(Value, 4);

    // Indication de version de format
    Value := FLMVersion;
    Stream.WriteBuffer(Value, 4);

    // Écriture des dimensions
    Dimensions := Map.Dimensions;
    Stream.WriteBuffer(Dimensions, sizeof(T3DPoint));

    // Préparation de la palette (Screws.Tag) et écriture de celle-ci
    for I := 0 to Master.ScrewCount-1 do
      Master.Screws[I].Tag := -1;
    Count := 0;
    Stream.WriteBuffer(Count, 4); // On repassera changer çà plus tard
    for I := 0 to Map.LinearMapCount-1 do with Map.LinearMap[I] do
    begin
      if Tag < 0 then
      begin
        Tag := Count;
        WriteStrToStream(Stream, ID);
        inc(Count);
      end;
    end;

    // Écriture de la carte
    if Count <= 16 then WritingIndex := 3 else
    if Count <= 256 then WritingIndex := 1 else
    WritingIndex := 0;

    for I := 0 to Map.LinearMapCount-1 do
    begin
      // Ajouter une case à écrire
      Writing[WritingIndex] := Map.LinearMap[I].Tag;

      // Passer à la case à écrire suivante
      dec(WritingIndex);

      WriteFurther;
    end;
  finally
    Stream.Free;
  end;
end;

initialization
  TMasterFile.RegisterMapFileClass(FLMMIMEType, TFLMMapFile);
end.

