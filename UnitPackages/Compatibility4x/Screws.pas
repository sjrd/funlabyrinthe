{*
  Décrit les composants de cases
  L'unité Screws regroupe les différents composants de cases de compatibilité
  4.x de FunLabyrinthe.
  @author Sébastien Jean Robert Doeraene
  @version 5.0
*}
unit Screws;

interface

uses
  SysUtils, Graphics, ScUtils, FunLabyUtils;

resourcestring
  sSunkButton = 'Bouton enfoncé'; /// Nom du bouton enfoncé
  sButton = 'Bouton n°%d';        /// Nom du bouton
  sButtonTemplate = 'Bouton';     /// Bouton modèle

const {don't localize}
  idSunkButton = 'SunkButton';         /// ID du bouton enfoncé
  idButton = 'Button%d';               /// ID du bouton
  idButtonTemplate = 'ButtonTemplate'; /// ID du bouton modèle

const {don't localize}
  fSunkButton = 'SunkButton'; /// Fichier du bouton enfoncé
  fButton = 'Button';         /// Fichier du bouton

implementation

end.

