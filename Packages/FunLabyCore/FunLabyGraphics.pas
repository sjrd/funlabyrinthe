{*
  Gestion des graphismes de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyGraphics;

interface

uses
  SysUtils, Classes, Contnrs, Graphics, GR32;

const
  DefaultDelay = 100;
  MaxTimeToFrameLength = $100;

  /// Couleur de transparence pour les fichiers .bmp
  clBmpTransparent32 = TColor32($FF008080);

  /// Couleur transparente
  clTransparent32 = TColor32($00000000);

type
  TAnimatedBitmap32 = class;

  {*
    Frame d'un bitmap 32 anim�
    @author sjrd
    @version 5.0
  *}
  TBitmap32Frame = class(TBitmap32)
  private
    FOwner: TAnimatedBitmap32; /// Bitmap anim� propri�taire

    FDelay: Integer; /// Temps d'affichage de ce frame en ms

    procedure SetDelay(Value: Integer);
  public
    constructor CreateFrame(AOwner: TAnimatedBitmap32);

    procedure Changed; override;

    property Delay: Integer read FDelay write SetDelay;
  end;

  {*
    Bitmap 32 anim�
    @author sjrd
    @version 5.0
  *}
  TAnimatedBitmap32 = class(TBitmap32Frame)
  private
    FFrames: TObjectList; /// Frames
    FTotalTime: Cardinal; /// Temps d'affichage total

    FTimeToFrameDivisor: Cardinal; /// Diviseur du temps pour FTimeToFrame
    FTimeToFrame: array of TBitmap32Frame; /// Acc�s temps vers frame

    procedure UpdateTimeToFrame;

    function GetFrameCount: Integer;
    procedure SetFrameCount(Value: Integer);
    function GetFrames(Index: Integer): TBitmap32Frame;
  protected
    procedure ChangeSize(var Width, Height: Integer;
      NewWidth, NewHeight: Integer); override;

    procedure AssignFromAnimatedBitmap32(ASource: TPersistent);
    procedure AssignFromGIFImage(ASource: TPersistent);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Changed; override;

    function GetFrameAtTime(TickCount: Cardinal): TBitmap32Frame;

    procedure DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32); overload;
    procedure DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
      DstX, DstY: Integer; const SrcRect: TRect); overload;
    procedure DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
      DstX, DstY: Integer); overload;
    procedure DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
      const DstRect: TRect); overload;
    procedure DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
      const DstRect, SrcRect: TRect); overload;

    property FrameCount: Integer read GetFrameCount write SetFrameCount;
    property Frames[Index: Integer]: TBitmap32Frame read GetFrames;

    property TotalTime: Cardinal read FTotalTime;
  end;

procedure HandleBmpTransparent(Bitmap: TBitmap32);

function LoadBitmapFromFile(const FileName: TFileName): TBitmap32;

implementation

uses
  GraphicEx, GraphicStrings, GIFImage;

{-----------------}
{ Global routines }
{-----------------}

{*
  Plus Grand Commun Diviseur
  @return PGCD de A et B
*}
function GCD(A, B: Cardinal): Cardinal;
var
  Temp: Cardinal;
begin
  while B <> 0 do
  begin
    Temp := B;
    B := A mod B;
    A := Temp;
  end;

  Result := A;
end;

{*
  Convertit la couleur de transparence des .bmp en transparent r�el
  @param Bitmap   Bitmap � traiter
*}
procedure HandleBmpTransparent(Bitmap: TBitmap32);
var
  X, Y: Integer;
  Line: PColor32Array;
begin
  for Y := 0 to Bitmap.Height-1 do
  begin
    Line := Bitmap.ScanLine[Y];

    for X := 0 to Bitmap.Width-1 do
      if Line[X] = clBmpTransparent32 then
        Line[X] := clTransparent32;
  end;
end;

{*
  Charge un bitmap 32 depuis un fichier, avec les amendements de FunLabyrinthe
  @param FileName   Nom du fichier image
  @return Bitmap 32 (ou bitmap 32 anim�) repr�sentant le fichier
*}
function LoadBitmapFromFile(const FileName: TFileName): TBitmap32;
var
  Picture: TPicture;
begin
  Result := nil;
  try
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(FileName);

      if (Picture.Graphic is TGIFImage) and
        (TGIFImage(Picture.Graphic).Images.Count > 1) then
        Result := TAnimatedBitmap32.Create
      else
        Result := TBitmap32.Create;

      Result.Assign(Picture);

      if Picture.Graphic.ClassType = TBitmap then
        HandleBmpTransparent(Result);
    finally
      Picture.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{----------------------}
{ TBitmap32Frame class }
{----------------------}

{*
  Cr�e un cadre de bitmap 32 anim�
  @param AOwner   Bitmap anim� propri�taire
*}
constructor TBitmap32Frame.CreateFrame(AOwner: TAnimatedBitmap32);
begin
  FOwner := AOwner;
  FDelay := DefaultDelay;

  inherited Create;
end;

{*
  Modifie le d�lai d'affichage de ce frame
  @parma Value   Nouveau d�lai d'affichage
*}
procedure TBitmap32Frame.SetDelay(Value: Integer);
begin
  if (Value > 0) and (Value <> FDelay) then
  begin
    FDelay := Value;
    Changed;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBitmap32Frame.Changed;
begin
  inherited;

  if (UpdateCount = 0) and (FOwner <> Self) then
    FOwner.Changed;
end;

{-------------------------}
{ TAnimatedBitmap32 class }
{-------------------------}

{*
  [@inheritDoc]
*}
constructor TAnimatedBitmap32.Create;
begin
  FFrames := TObjectList.Create;
  FFrames.Add(Self);

  inherited CreateFrame(Self);
end;

{*
  [@inheritDoc]
*}
destructor TAnimatedBitmap32.Destroy;
begin
  inherited;

  FFrames.Extract(Self);
  FFrames.Free;
end;

{*
  Met � jour la table FTimeToFrame
*}
procedure TAnimatedBitmap32.UpdateTimeToFrame;
var
  I, J, Index: Integer;
  GCDOfDelays, TimeToFrameLength: Cardinal;
  Frame: TBitmap32Frame;
begin
  FTotalTime := Delay;
  GCDOfDelays := Delay;

  for I := 1 to FrameCount-1 do
  begin
    Frame := Frames[I];
    Inc(FTotalTime, Frame.Delay);
    GCDOfDelays := GCD(GCDOfDelays, Frame.Delay);
  end;

  TimeToFrameLength := FTotalTime div GCDOfDelays;

  if TimeToFrameLength > MaxTimeToFrameLength then
  begin
    FTimeToFrameDivisor := 0;
    FTimeToFrame := nil;
  end else
  begin
    FTimeToFrameDivisor := GCDOfDelays;
    SetLength(FTimeToFrame, TimeToFrameLength);

    Index := 0;

    for I := 0 to FrameCount-1 do
    begin
      for J := 0 to Frames[I].Delay div FTimeToFrameDivisor - 1 do
      begin
        FTimeToFrame[Index] := Frames[I];
        Inc(Index);
      end;
    end;
  end;
end;

{*
  Nombre de frames
  @return Nombre de frames
*}
function TAnimatedBitmap32.GetFrameCount: Integer;
begin
  Result := FFrames.Count;
end;

{*
  Modifie le nombre de frames
  @param Value   Nouveau nombre de cadres
*}
procedure TAnimatedBitmap32.SetFrameCount(Value: Integer);
var
  I: Integer;
begin
  if Value < 1 then
    Value := 1;

  FFrames.Count := Value;

  for I := Value-1 downto 1 do
  begin
    if FFrames[I] <> nil then
      Break;

    FFrames[I] := TBitmap32Frame.CreateFrame(Self);
    TBitmap32Frame(FFrames[I]).SetSize(Width, Height);
  end;
end;

{*
  Tableau zero-based des frames
  @param Index   Index d'un frame
  @return Frame � l'index sp�cifi�
*}
function TAnimatedBitmap32.GetFrames(Index: Integer): TBitmap32Frame;
begin
  Result := TBitmap32Frame(FFrames[Index]);
end;

{*
  [@inheritDoc]
*}
procedure TAnimatedBitmap32.ChangeSize(var Width, Height: Integer;
  NewWidth, NewHeight: Integer);
var
  I: Integer;
begin
  inherited;

  for I := 1 to FrameCount-1 do
    Frames[I].SetSize(NewWidth, NewHeight);
end;

{*
  Assigne depuis un bitmap 32 anim�
  @param ASource   Source
*}
procedure TAnimatedBitmap32.AssignFromAnimatedBitmap32(ASource: TPersistent);
var
  Source: TAnimatedBitmap32;
  I: Integer;
begin
  Source := ASource as TAnimatedBitmap32;

  BeginUpdate;
  try
    SetSize(Source.Width, Source.Height);
    FrameCount := Source.FrameCount;

    inherited Assign(Source);

    for I := 1 to FrameCount-1 do
      Frames[I].Assign(Source.Frames[I]);
  finally
    EndUpdate;
  end;
end;

{*
  Assigne depuis une image GIF
  @param ASource   Source
*}
procedure TAnimatedBitmap32.AssignFromGIFImage(ASource: TPersistent);
var
  Source: TGIFImage;
  I: Integer;
begin
  Source := ASource as TGIFImage;

  BeginUpdate;
  try
    SetSize(Source.Width, Source.Height);
    FrameCount := Source.Images.Count;

    for I := 0 to FrameCount-1 do
      Frames[I].Assign(Source.Images[I].Bitmap);
  finally
    EndUpdate;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TAnimatedBitmap32.Assign(Source: TPersistent);
begin
  if Source is TAnimatedBitmap32 then
    AssignFromAnimatedBitmap32(Source)
  else if Source is TGIFImage then
    AssignFromGIFImage(Source)
  else if Source is TPicture then
  begin
    if TPicture(Source).Graphic is TGIFImage then
      AssignFromGIFImage(TPicture(Source).Graphic)
    else
      inherited;
  end else
    inherited;
end;

{*
  [@inheritDoc]
*}
procedure TAnimatedBitmap32.Changed;
begin
  UpdateTimeToFrame;

  inherited;
end;

{*
  Trouve le frame correspondant � un temps donn�
  @param TickCount   Tick count
  @return Frame � l'instant TickCount
*}
function TAnimatedBitmap32.GetFrameAtTime(TickCount: Cardinal): TBitmap32Frame;
var
  I: Integer;
begin
  TickCount := TickCount mod TotalTime;

  if FTimeToFrameDivisor <> 0 then
    Result := FTimeToFrame[TickCount div FTimeToFrameDivisor]
  else
  begin
    for I := 0 to FrameCount-1 do
    begin
      Result := Frames[I];
      if TickCount < Result.Delay then
        Exit;
      Dec(TickCount, Result.Delay);
    end;

    Assert(False);
    Result := Self;
  end;
end;

{*
  Dessine le bitmap � un temps donn�
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst);
end;

{*
  Dessine le bitmap � un temps donn�
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  DstX, DstY: Integer; const SrcRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstX, DstY, SrcRect);
end;

{*
  Dessine le bitmap � un temps donn�
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  DstX, DstY: Integer);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstX, DstY);
end;

{*
  Dessine le bitmap � un temps donn�
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  const DstRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstRect);
end;

{*
  Dessine le bitmap � un temps donn�
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  const DstRect, SrcRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstRect, SrcRect);
end;

initialization
  FileFormatList.UnregisterFileFormat('gif', nil);
  FileFormatList.RegisterFileFormat('gif', gesCompuserve, '',
    [ftRaster, ftMultiImage, ftAnimation], False, True, TGIFImage);
finalization
  FileFormatList.UnregisterFileFormat('', TGIFImage);
end.

