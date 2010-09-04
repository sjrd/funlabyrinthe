unit FunLabyEditConsts;

interface

resourcestring
  SErrorTitle = 'Erreur';
  SFatalErrorTitle = 'Erreur fatale';

  SBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalités coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  SFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore implémentée';

  SDefaultPlayerName = 'Joueur';

  SPlayerPosition = 'Position : %s';
  SCenterToPosition = 'Centrer la vue sur ce joueur';
  SShowPlugins = 'Plug-in...';
  SShowAttributes = 'Attributs...';
  SShowObjects = 'Objets...';

  SConfirmExitTitle = 'Enregistrer le fichier';
  SConfirmExit = 'Le fichier a été modifié. Voulez-vous l''enregistrer ?';

  SCantSave = 'Impossible d''enregistrer';
  SCantSaveUnplacedPlayer =
    'Impossible d''enregistrer car le joueur d''ID %s n''a pas été placé';

  SCantCenterToPosition = 'Impossible de centrer le joueur';
  SCantCenterToUnplacedPlayer =
    'Impossible de centrer sur ce joueur car il n''a pas été placé';

  SOnlyFieldOutsideTitle = 'Remplacer l''extérieur de la carte';
  SOnlyFieldOutside =
    'Seuls les terrains sont acceptés en dehors de la carte';

  SRemoveMapTitle = 'Supprimer une carte';
  SRemoveMap =
    'Êtes-vous certain de vouloir supprimer complètement cette carte ?';

  SDuplicateSourceTitle = 'Fichier source déjà présent';
  SDuplicateSource = 'Ce fichier source est déjà présent dans le projet';

  SRemoveSourceTitle = 'Retirer un fichier source';
  SRemoveSource =
    'Êtes-vous certain de vouloir retirer ce fichier source du projet ?';
  SErrorWhileOpeningSourceFile =
    'Une erreur est survenu lors de l''ouverture du fichier avec le message '+
    '"%s". Voulez-vous retirer ce fichier source du projet ?';

  SAllUnitTypes = 'Tous les types d''unité';

  SDuplicateUnitTitle = 'Unité déjà présente';
  SDuplicateUnit = 'Il y a déjà une unité ''%s''';

  SRemoveUnitTitle = 'Supprimer une unité';
  SCantRemoveLastUnit = 'Il faut toujours garder au moins une unité';
  SConfirmRemoveUnit = 'Êtes-vous sûr-e de vouloir supprimer l''unité ''%s'' ?';

  SComponentDataTitle = 'Propriétés du composant';
  SPlayerDataTitle = 'Propriétés par joueur';
  SPlayerAttributesTitle = 'Attributs du joueur';
  SPlayerPluginsTitle = 'Plug-in attachés au joueur';

  SCollectionItemTitle = '%d - %s';

  SCreateCompChooseIDTitle = 'Création d''un nouveau composant';
  SCreateCompChooseID = 'Veuillez choisir l''ID du nouveau composant';

  SInvalidComponentIDTitle = 'ID de composant invalide';
  SBadComponentID =
    'L''ID ne doit contenir que des lettres non accentuées et des chiffres';
  SComponentAlreadyExists = 'Il existe déjà un composant avec cet ID';

  SInvalidFileNameTitle = 'Nom de fichier invalide';
  SInvalidUnitName =
    '%s n''est pas un nom d''unité valide, il ne doit contenir que des '+
    'lettres non accentuées et des chiffres, et commencer par une lettre';
  SInvalidExtension =
    'L''extension %s n''est pas valide pour ce type de fichier';

  SNoFieldTitle = 'Aucun terrain';
  SNoField =
    'Impossible de lancer cette opération car ce labyrinthe ne contient '+
    'aucun terrain';

  SResizeErrorTitle = 'Erreur de redimensionnement';
  SCantResizeToEmpty = 'Impossible de rétrécir plus la carte';

  SConfirmDeleteComponentTitle = 'Supprimer le composant';
  SConfirmDeleteComponent =
    'Êtes-vous sûr-e de vouloir supprimer ce composant ?';

implementation

end.

