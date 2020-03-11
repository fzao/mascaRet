Module M_LecParamSedim_I

Interface

Subroutine  LecParamSedim ( &

 UniteListing   , & ! Unite logique fichier listing
 FichierSedim   , & ! Fichier contenant les donnees sédim. rel. à ch. couche + donnees concernant la stabilite des berges
 ImpressionSedim   , &
 NbCouches    , & ! Nb de couches sedimentaires
 CoucheSed    , & ! variable contenant ttes les donnees rel. à ch. couche
 Talus     , & ! variable contenant ttes les donnees rel. aux talus
 LimiteSable    , & ! % de sable à part. dql la couche est traitee suivant les lois du sable
 CnuxV     , & ! Coefficient de diffusion vases
 CnuxS     , & ! Coefficient de diffusion sables
 ConsConv                , & ! paramètres schéma convection
! FracH    , & ! MS2018: nouvelle variable, clipping evolution pourcentage de la hauteur d eau
 MOTINT     , &
 MOTREA     , &
 MOTCAR     , &
 ADRESS     , &
 MOTLOG                  , &
    Erreur     )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER, F. DELHOPITAL M. Jodeau
!
!  VERSION : 5.1       08-2009  Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!  Fonction : Lecture des caracteristiques des sediments en place
!  --------
!
!  Sous-programme appelant : Pretrait_Courlis
!  -----------------------
!
!  Sous-programme appele : - LecFichierSedim
!  ---------------------
!
!=========================================================================


!=========================================================================
! DECLARATIONS
!=========================================================================

use M_PRECISION    ! Definition de la precision DOUBLE ou SIMPLE
use M_CONSTANTES_CALCUL_C ! Constantes num, phys et info
use M_FICHIER_T    ! Definition du type FICHIER_T
use M_COUCHE_T    ! Definition du type COUCHE_T
use M_TALUS_T    ! Definition du type TALUS_T
use M_LecFichierSedim_I  ! Interface de sous-programme
use M_CONSTANTES_TRACER_T ! paramètres schema convection

use M_ERREUR_T            ! Type ERREUR_T
use M_MESSAGE_C           ! Messages d'erreur
use M_TRAITER_ERREUR_I    ! Traitement de l'erreur

!.. Implicit Declarations ..
  implicit none

! Variables d'entrée
  integer       , intent(in   ) :: UniteListing
  logical       , intent(in   ) :: ImpressionSedim

  integer           , dimension(:)  , intent(in   ) :: MOTINT
  logical          , dimension(:)  , intent(in   ) :: MOTLOG
  real(DOUBLE)      , dimension(:)  , intent(in   ) :: MOTREA
  character(LEN=144), dimension(:)  , intent(in   ) :: MOTCAR
  integer           , dimension(:,:), intent(in   ) :: ADRESS

  type(FICHIER_T)     , intent(inout) :: FichierSedim


! Variables de sortie
  integer        ,intent(  out) :: NbCouches
  type(TALUS_T)       ,intent(  out) :: Talus
  real(DOUBLE)       ,intent(  out) :: LimiteSable
  real(DOUBLE)        ,intent(  out) :: CnuxV
  real(DOUBLE)        ,intent(  out) :: CnuxS
  type(COUCHE_T), dimension(:), pointer    :: CoucheSed
  type(CONSTANTES_TRACER_T), intent(out) :: ConsConv

! MS2018: Nouvelles variables
!  real(DOUBLE)        ,intent(  out) :: FracH

! Variables locales
! PU2017 : Mise en commentaire des variables locales
!  integer    :: ModeParamSedim ! Mode d'entrée des paramètres sédimentaires
         ! (par fichier ou par l'interface)
!  integer    :: iCouche   ! Compteur de couche
!  real(DOUBLE) :: ValSum   ! Sum of values for making tests


! Traitement des erreurs
!  integer   :: retour       ! Code de retour de la fonction read  ! PU2017 : Mis en commentaire
!  character(132) :: arbredappel_old ! Arbre d'appel initial  ! PU2017 : Mis en commentaire
  type(ERREUR_T), intent(inout) :: Erreur

!=========================================================================

End Subroutine LecParamSedim

End Interface

End Module M_LecParamSedim_I

