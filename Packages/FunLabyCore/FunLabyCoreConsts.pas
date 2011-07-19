{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.2~beta2'; /// Version courante de FunLabyrinthe
  CurrentMinorVersion = 1;      /// Version mineure courante de FunLabyrinthe
  FunLabyAuthorName = 'Sébastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://www.funlabyrinthe.com/';     /// Site Web

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstrué par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';
  SDefaultPlayerName = 'Joueur';
  SComponentCreatorHint = 'Créer un nouveau composant de type %s';

  // Noms des catégories standard
  SCategoryFields = 'Terrains';
  SCategoryEffects = 'Effets';
  SCategoryTools = 'Outils';
  SCategoryObstacles = 'Obstacles';
  SCategoryNeutrals = 'Neutres';
  SCategoryVehicles = 'Véhicules';
  SCategoryPlayers = 'Joueurs';
  SCategoryObjects = 'Objets';
  SCategoryPlugins = 'Plug-in';
  SCategoryComponentCreators = 'Composants supplémentaires';

  // Erreurs de composants
  SComponentNotFoundTitle = 'Composant non trouvé';
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SInvalidComponentTitle = 'Composant invalide';
  SInvalidComponent = 'Le composant d''ID %s n''est pas valide';
  SRepareComponentError =
    'Voulez-vous ignorer cette erreur et tenter de continuer ?';
  SEditingRequiredForSetID =
    'Impossible de modifier l''ID d''un composant sauf en édition';
  SAdditionnalRequiredForSetID =
    'Impossible de modifier l''ID d''un composant créé automatiquement';
  SInvalidID = '''%s'' n''est pas un ID de composant valide (seuls les '+
    'lettres non accentuées, les chiffres et le _ sont autorisés)';
  SDuplicateID = 'Il existe déjà un composant dont l''ID est %s';
  SUnsupportedCommand = 'La commande %s n''est pas supportée';
  STemporaryStatedMap =
    'La carte d''ID %s est dans un état temporaire qui ne peut être enregistré';

  // Titres des messages
  SDescription = 'Description';
  SMessage = 'Message';
  STip = 'Indice';
  SChoice = 'Choix';
  SError = 'Erreur';
  SFailure = 'Échec';
  SBlindAlley = 'Impasse';
  SWon = 'Gagné !';
  SLost = 'Perdu !';

  // Formats de fichier et traitement des fichiers
  SFullVersionNumber = '%s (build %d)';
  SInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  SVersionTooHigh = 'Le fichier a été enregistré avec une version ultérieure '+
    'de FunLabyrinthe (v%s). Il ne peut être ouvert.';
  SFileNotFoundTitle = 'Fichier introuvable';
  SFileNotFound = 'Le fichier spécifié "%s" n''existe pas';
  SCannotMakeHRef = 'Impossible de construire un href pour le fichier ''%s'' '+
    'car celui-ci n''est pas dans le chemin de recherche du projet';
  SResourceNotFound = 'La ressource spécifiée "%s" n''existe pas';
  SCannotMakeResourceHRef = 'Impossible de construire un href pour la '+
    'ressource ''%s'' car celle-ci n''est pas dans le chemin de recherche du '+
    'projet';
  SUnknownUnitType = 'Type d''unité ''%s'' inconnu';
  SThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  SEditingNotAllowed = 'L''édition de ce fichier n''est pas permise';
  SCantEditSaveguard = 'L''édition d''une sauvegarde est impossible';
  SNoFileName = 'Aucun nom de fichier spécifié';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';
  SPainterFilter = 'Peintre FunLabyrinthe|*.pnt';

implementation

end.

