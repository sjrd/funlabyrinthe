{*
  Gestion des graphismes de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyGraphics;

interface

uses
  Types, SysUtils, Classes, Contnrs, Graphics, GR32;

const
  DefaultDelay = 100;
  MaxTimeToFrameLength = $100;

  /// Couleur de transparence pour les fichiers .bmp
  clBmpTransparent32 = TColor32($FF008080);

  /// Couleur transparente
  clTransparent32 = TColor32($00000000);

  /// Couleur magique qui force la valeur du canal Alpha
  clMagicForceAlpha32 = TColor32($00123213);

type
  TAnimatedBitmap32 = class;

  {*
    Frame d'un bitmap 32 animé
    @author sjrd
    @version 5.0
  *}
  TBitmap32Frame = class(TBitmap32)
  private
    FOwner: TAnimatedBitmap32; /// Bitmap animé propriétaire

    FDelay: Integer; /// Temps d'affichage de ce frame en ms

    procedure SetDelay(Value: Integer);
  public
    constructor CreateFrame(AOwner: TAnimatedBitmap32);

    procedure Assign(Source: TPersistent); override;

    procedure Changed; override;

    property Delay: Integer read FDelay write SetDelay;
  end;

  {*
    Bitmap 32 animé
    @author sjrd
    @version 5.0
  *}
  TAnimatedBitmap32 = class(TBitmap32Frame)
  private
    FFrames: TObjectList; /// Frames
    FTotalTime: Cardinal; /// Temps d'affichage total

    FTimeToFrameDivisor: Cardinal; /// Diviseur du temps pour FTimeToFrame
    FTimeToFrame: array of TBitmap32Frame; /// Accès temps vers frame

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

function GCD(A, B: Cardinal): Cardinal;
function LCM(A, B: Cardinal): Cardinal;

procedure ReplaceColorInBitmap32(Bitmap: TBitmap32; FromColor: TColor32;
  ToColor: TColor32);
procedure HandleBmpTransparent(Bitmap: TBitmap32);

function MultiplyComponents(Left, Right: TColor32): TColor32;

function LoadBitmapFromFile(const FileName: TFileName): TBitmap32;

function AnimateBitmap(Bitmap: TBitmap32;
  const FrameDelays: array of Cardinal): TAnimatedBitmap32; overload;
function AnimateBitmap(Bitmap: TBitmap32; FrameCount: Integer;
  FrameDelay: Cardinal): TAnimatedBitmap32; overload;
function AnimateBitmap(Bitmap: TBitmap32;
  FrameDelay: Cardinal): TAnimatedBitmap32; overload;

procedure DrawRepeat(Dest, Src: TBitmap32; const DestRect, SrcRect: TRect;
  TickCount: Cardinal = 0);

procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32); overload;
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; DstX, DstY: Integer; const SrcRect: TRect); overload;
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; DstX, DstY: Integer); overload;
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; const DstRect: TRect); overload;
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; const DstRect, SrcRect: TRect); overload;

function NeedsMagicBlend(Bitmap: TBitmap32): Boolean;
procedure InstallMagicBlend(Bitmap: TBitmap32);
procedure InstallMagicBlendIfNeeded(Bitmap: TBitmap32);

var
  FunLabyMagicBlend: TPixelCombineEvent;

implementation

uses
  GraphicEx, GraphicStrings, GIFImage, GR32_Blend, ScUtils;

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
  Plus Petit Commun Multiple
  @return PPCM de A et B
*}
function LCM(A, B: Cardinal): Cardinal;
begin
  Result := A * B div GCD(A, B);
end;

{*
  Remplace toute occurrence d'une couleur par une autre dans un bitmap 32
  @param Bitmap      Bitmap à traiter
  @param FromColor   Couleur à remplacer
  @param ToColor     Couleur de remplacement
*}
procedure ReplaceColorInBitmap32(Bitmap: TBitmap32; FromColor: TColor32;
  ToColor: TColor32);
var
  X, Y: Integer;
  Line: PColor32Array;
begin
  for Y := 0 to Bitmap.Height-1 do
  begin
    Line := Bitmap.ScanLine[Y];

    for X := 0 to Bitmap.Width-1 do
      if Line[X] = FromColor then
        Line[X] := ToColor;
  end;
end;

{*
  Convertit la couleur de transparence des .bmp en transparent réel
  @param Bitmap   Bitmap à traiter
*}
procedure HandleBmpTransparent(Bitmap: TBitmap32);
begin
  ReplaceColorInBitmap32(Bitmap, clBmpTransparent32, clTransparent32);
end;

{*
  Multiplie les composantes de deux couleurs deux à deux
  @param Left    Première couleur
  @param Right   Seconde couleur
  @return Résultat de la multiplication deux à deux des composantes
*}
function MultiplyComponents(Left, Right: TColor32): TColor32;
var
  A: TColor32Entry absolute Left;
  B: TColor32Entry absolute Right;
  R: TColor32Entry absolute Result;
begin
  R.B := Word(A.B) * Word(B.B) div 255;
  R.G := Word(A.G) * Word(B.G) div 255;
  R.R := Word(A.R) * Word(B.R) div 255;
  R.A := Word(A.A) * Word(B.A) div 255;
end;

{*
  Assigne un frame GIF à un bitmap 32
  @param Dest     Bitmap 32 destination
  @param Source   Frame GIF source
*}
procedure AssignGIFSubImageToBitmap32(Dest: TBitmap32; Source: TGIFSubImage);
begin
  Dest.SetSize(Source.Width, Source.Height);

  Source.Draw(Dest.Canvas, Dest.BoundsRect, False, False);
  Dest.ResetAlpha;

  if Source.Transparent then
  begin
    ReplaceColorInBitmap32(Dest,
      Color32(Source.GraphicControlExtension.TransparentColor),
      clTransparent32);
  end;

  if Dest is TBitmap32Frame then
    TBitmap32Frame(Dest).Delay := Source.GraphicControlExtension.Delay * 10;
end;

{*
  Charge un bitmap 32 depuis un fichier, avec les amendements de FunLabyrinthe
  @param FileName   Nom du fichier image
  @return Bitmap 32 (ou bitmap 32 animé) représentant le fichier
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

      if Picture.Graphic is TGIFImage then
      begin
        if TGIFImage(Picture.Graphic).Images.Count = 1 then
        begin
          Result := TBitmap32.Create;
          AssignGIFSubImageToBitmap32(Result,
            TGIFImage(Picture.Graphic).Images[0]);
        end else
        begin
          Result := TAnimatedBitmap32.Create;
          Result.DrawMode := dmBlend;    // need to set those before Assign
          Result.CombineMode := cmMerge; // sets FrameCount
          Result.Assign(Picture);
        end;
      end else
      begin
        Result := TBitmap32.Create;
        Result.Assign(Picture);

        if Picture.Graphic.ClassType = TBitmap then
          HandleBmpTransparent(Result);
      end;
    finally
      Picture.Free;
    end;

    Result.DrawMode := dmBlend;
    Result.CombineMode := cmMerge;

    InstallMagicBlendIfNeeded(Result);
  except
    Result.Free;
    raise;
  end;
end;

{*
  Anime un bitmap
  AnimateBitmap transforme un bitmap inanimé en un bitmap animé. Le bitmap de
  base est découpé en frames dans sa largeur.
  @param Bitmap        Bitmap inanimé de base
  @param FrameDelays   Tableau des délais d'affichage de chaque frame
  @return Bitmap animé construit
*}
function AnimateBitmap(Bitmap: TBitmap32;
  const FrameDelays: array of Cardinal): TAnimatedBitmap32;
var
  FrameCount, Width, Height, I: Integer;
  SrcRect: TRect;
  Frame: TBitmap32Frame;
begin
  FrameCount := Length(FrameDelays);
  Width := Bitmap.Width div FrameCount;
  Height := Bitmap.Height;

  SrcRect := Rect(0, 0, Width, Height);

  Result := TAnimatedBitmap32.Create;
  try
    Result.SetSize(Width, Height);
    Result.DrawMode := Bitmap.DrawMode;
    Result.CombineMode := Bitmap.CombineMode;
    Result.FrameCount := FrameCount;

    for I := 0 to FrameCount-1 do
    begin
      Frame := Result.Frames[I];
      Frame.Delay := FrameDelays[I];
      Frame.Clear(clTransparent32);
      Frame.Draw(0, 0, SrcRect, Bitmap);

      OffsetRect(SrcRect, Width, 0);
    end;
  except
    Result.Free;
    raise;
  end;
end;

{*
  Anime un bitmap
  AnimateBitmap transforme un bitmap inanimé en un bitmap animé. Le bitmap de
  base est découpé en frames dans sa largeur.
  @param Bitmap       Bitmap inanimé de base
  @param FrameCount   Nombre de frames
  @param FrameDelay   Délai d'affichage, appliqué à toutes les frames
  @return Bitmap animé construit
*}
function AnimateBitmap(Bitmap: TBitmap32; FrameCount: Integer;
  FrameDelay: Cardinal): TAnimatedBitmap32;
var
  FrameDelays: TCardinalDynArray;
  I: Integer;
begin
  SetLength(FrameDelays, FrameCount);
  for I := 0 to FrameCount-1 do
    FrameDelays[I] := FrameDelay;

  Result := AnimateBitmap(Bitmap, FrameDelays);
end;

{*
  Anime un bitmap
  AnimateBitmap transforme un bitmap inanimé en un bitmap animé. Le bitmap de
  base est découpé en frames dans sa largeur.
  Le nombre de frames est déterminé par formule
    (Bitmap.Width div Bitmap.Height).
  @param Bitmap       Bitmap inanimé de base
  @param FrameDelay   Délai d'affichage, appliqué à toutes les frames
  @return Bitmap animé construit
*}
function AnimateBitmap(Bitmap: TBitmap32;
  FrameDelay: Cardinal): TAnimatedBitmap32;
begin
  Result := AnimateBitmap(Bitmap, Bitmap.Width div Bitmap.Height, FrameDelay);
end;

{*
  Dessine un bitmap sur un autre, avec répétition de la source
  @param Dest        Bitmap destination
  @param Src         Bitmap source
  @param DestRect    Rectangle destination
  @param SrcRect     Rectangle source
  @param TickCount   Tick count auquel dessiner le bitmap source (si animé)
*}
procedure DrawRepeat(Dest, Src: TBitmap32; const DestRect, SrcRect: TRect;
  TickCount: Cardinal = 0);
var
  OldDestClip: TRect;
  SrcWidth, SrcHeight, X, Y: Integer;
  SrcAnimated: Boolean;
begin
  if IsRectEmpty(DestRect) or IsRectEmpty(SrcRect) then
    Exit;

  OldDestClip := Dest.ClipRect;
  Dest.BeginUpdate;
  try
    Dest.ClipRect := DestRect;
    SrcWidth := SrcRect.Right - SrcRect.Left;
    SrcHeight := SrcRect.Bottom - SrcRect.Top;
    SrcAnimated := (TickCount <> 0) and (Src is TAnimatedBitmap32);

    X := DestRect.Left;
    while X < DestRect.Right do
    begin
      Y := DestRect.Top;
      while Y < DestRect.Bottom do
      begin
        if SrcAnimated then
          TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dest, X, Y, SrcRect)
        else
          Src.DrawTo(Dest, X, Y, SrcRect);

        Inc(Y, SrcHeight);
      end;
      Inc(X, SrcWidth);
    end;
  finally
    Dest.ClipRect := OldDestClip;
    Dest.EndUpdate;
    Dest.Changed(DestRect);
  end;
end;

{*
  Dessine un bitmap sur un autre à un temps donné
  Si le bitmap source est un bitmap animé (TAnimatedBitmap32), alors sa méthode
  DrawAtTimeTo est utilisée. Sinon, sa méthode DrawTo est utilisée.
  @param Src         Bitmap source
  @param TickCount   Tick-count
  @param Dst         Bitmap destination
*}
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32);
begin
  if (TickCount <> 0) and (Src is TAnimatedBitmap32) then
    TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dst)
  else
    Src.DrawTo(Dst);
end;

{*
  Dessine un bitmap sur un autre à un temps donné
  Si le bitmap source est un bitmap animé (TAnimatedBitmap32), alors sa méthode
  DrawAtTimeTo est utilisée. Sinon, sa méthode DrawTo est utilisée.
  @param Src         Bitmap source
  @param TickCount   Tick-count
  @param Dst         Bitmap destination
*}
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; DstX, DstY: Integer; const SrcRect: TRect);
begin
  if (TickCount <> 0) and (Src is TAnimatedBitmap32) then
    TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dst, DstX, DstY, SrcRect)
  else
    Src.DrawTo(Dst, DstX, DstY, SrcRect);
end;

{*
  Dessine un bitmap sur un autre à un temps donné
  Si le bitmap source est un bitmap animé (TAnimatedBitmap32), alors sa méthode
  DrawAtTimeTo est utilisée. Sinon, sa méthode DrawTo est utilisée.
  @param Src         Bitmap source
  @param TickCount   Tick-count
  @param Dst         Bitmap destination
*}
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; DstX, DstY: Integer);
begin
  if (TickCount <> 0) and (Src is TAnimatedBitmap32) then
    TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dst, DstX, DstY)
  else
    Src.DrawTo(Dst, DstX, DstY);
end;

{*
  Dessine un bitmap sur un autre à un temps donné
  Si le bitmap source est un bitmap animé (TAnimatedBitmap32), alors sa méthode
  DrawAtTimeTo est utilisée. Sinon, sa méthode DrawTo est utilisée.
  @param Src         Bitmap source
  @param TickCount   Tick-count
  @param Dst         Bitmap destination
*}
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; const DstRect: TRect);
begin
  if (TickCount <> 0) and (Src is TAnimatedBitmap32) then
    TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dst, DstRect)
  else
    Src.DrawTo(Dst, DstRect);
end;

{*
  Dessine un bitmap sur un autre à un temps donné
  Si le bitmap source est un bitmap animé (TAnimatedBitmap32), alors sa méthode
  DrawAtTimeTo est utilisée. Sinon, sa méthode DrawTo est utilisée.
  @param Src         Bitmap source
  @param TickCount   Tick-count
  @param Dst         Bitmap destination
*}
procedure DrawBitmapAtTimeTo(Src: TBitmap32; TickCount: Cardinal;
  Dst: TBitmap32; const DstRect, SrcRect: TRect);
begin
  if (TickCount <> 0) and (Src is TAnimatedBitmap32) then
    TAnimatedBitmap32(Src).DrawAtTimeTo(TickCount, Dst, DstRect, SrcRect)
  else
    Src.DrawTo(Dst, DstRect, SrcRect);
end;

{*
  Teste si une couleur est une couleur magique pour forcer l'Alpha
  @param C   Couleur à tester
  @return True ssi C est une couleur magique pour forcer l'Alpha
*}
function IsMagicForceAlpha(C: TColor32): Boolean; inline;
begin
  Result := (C and $00FFFFFF) = clMagicForceAlpha32;
end;

{*
  Fusionne deux couleurs en tenant compte des valeurs magiques de FunLabyrinthe
  @param Self   Pas utilisé
  @param F      Couleur source, d'avant-plan
  @param B      Couleur destination, d'arrière-plan
  @param M      Couleur maître
*}
procedure _FunLabyMagicBlend(Self: Pointer; F: TColor32; var B: TColor32;
  M: TColor32);
begin
  if IsMagicForceAlpha(F) then
    B := (B and $00FFFFFF) or (F and $FF000000)
  else
    BlendMem(F, B);
end;

{*
  Teste si un bitmap a besoin du blend magique de FunLabyrinthe
  @param Bitmap   Bitmap à tester
  @return True ssi Bitmap a besoin du blend magique de FunLabyrinthe
*}
function NeedsMagicBlend(Bitmap: TBitmap32): Boolean;
var
  X, Y: Integer;
  Line: PColor32Array;
begin
  for Y := 0 to Bitmap.Height-1 do
  begin
    Line := Bitmap.ScanLine[Y];

    for X := 0 to Bitmap.Width-1 do
    begin
      if IsMagicForceAlpha(Line^[X]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

{*
  Installe le blend magique de FunLabyrinthe sur un bitmap
  @param Bitmap   Bitmap à patcher
*}
procedure InstallMagicBlend(Bitmap: TBitmap32);
begin
  Bitmap.DrawMode := dmCustom;
  Bitmap.OnPixelCombine := FunLabyMagicBlend;
end;

{*
  Installe le blend magique de FunLabyrinthe sur un bitmap si nécessaire
  @param Bitmap   Bitmap à patcher
*}
procedure InstallMagicBlendIfNeeded(Bitmap: TBitmap32);
var
  I: Integer;
begin
  if NeedsMagicBlend(Bitmap) then
    InstallMagicBlend(Bitmap);

  if Bitmap is TAnimatedBitmap32 then
    for I := 1 to TAnimatedBitmap32(Bitmap).FrameCount-1 do
      InstallMagicBlendIfNeeded(TAnimatedBitmap32(Bitmap).Frames[I]);
end;

{----------------------}
{ TBitmap32Frame class }
{----------------------}

{*
  Crée un cadre de bitmap 32 animé
  @param AOwner   Bitmap animé propriétaire
*}
constructor TBitmap32Frame.CreateFrame(AOwner: TAnimatedBitmap32);
begin
  FOwner := AOwner;
  FDelay := DefaultDelay;

  inherited Create;
end;

{*
  Modifie le délai d'affichage de ce frame
  @parma Value   Nouveau délai d'affichage
*}
procedure TBitmap32Frame.SetDelay(Value: Integer);
begin
  if (Value > 0) and (Value <> FDelay) then
  begin
    FDelay := Value;
    FOwner.UpdateTimeToFrame;
    Changed;
  end;
end;

{*
  [@inheritDoc]
*}
procedure TBitmap32Frame.Assign(Source: TPersistent);
begin
  inherited;

  if Source is TBitmap32Frame then
    Delay := TBitmap32Frame(Source).Delay;
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
  Met à jour la table FTimeToFrame
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
    with TBitmap32Frame(FFrames[I]) do
    begin
      SetSize(Self.Width, Self.Height);
      DrawMode := Self.DrawMode;
      CombineMode := Self.CombineMode;
      OnPixelCombine := Self.OnPixelCombine;
    end;
  end;

  UpdateTimeToFrame;
  Changed;
end;

{*
  Tableau zero-based des frames
  @param Index   Index d'un frame
  @return Frame à l'index spécifié
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
  Assigne depuis un bitmap 32 animé
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
    Changed;
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
      AssignGIFSubImageToBitmap32(Frames[I], Source.Images[I]);
  finally
    EndUpdate;
    Changed;
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
  Trouve le frame correspondant à un temps donné
  @param TickCount   Tick count
  @return Frame à l'instant TickCount
*}
function TAnimatedBitmap32.GetFrameAtTime(TickCount: Cardinal): TBitmap32Frame;
var
  I: Integer;
begin
  if FrameCount = 1 then
  begin
    Result := Self;
    Exit;
  end;

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
  Dessine le bitmap à un temps donné
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst);
end;

{*
  Dessine le bitmap à un temps donné
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  DstX, DstY: Integer; const SrcRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstX, DstY, SrcRect);
end;

{*
  Dessine le bitmap à un temps donné
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  DstX, DstY: Integer);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstX, DstY);
end;

{*
  Dessine le bitmap à un temps donné
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  const DstRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstRect);
end;

{*
  Dessine le bitmap à un temps donné
  @param TickCount   Tick count
*}
procedure TAnimatedBitmap32.DrawAtTimeTo(TickCount: Cardinal; Dst: TBitmap32;
  const DstRect, SrcRect: TRect);
begin
  GetFrameAtTime(TickCount).DrawTo(Dst, DstRect, SrcRect);
end;

initialization
  FunLabyMagicBlend := TPixelCombineEvent(MakeMethod(@_FunLabyMagicBlend));

  FileFormatList.UnregisterFileFormat('gif', nil);
  FileFormatList.RegisterFileFormat('gif', gesCompuserve, '',
    [ftRaster, ftMultiImage, ftAnimation], False, True, TGIFImage);
finalization
  FileFormatList.UnregisterFileFormat('', TGIFImage);
end.

