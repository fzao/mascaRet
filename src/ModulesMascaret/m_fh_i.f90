!== Copyright (C) 2000-2020 EDF-CEREMA ==
!
!   This file is part of MASCARET.
!
!   MASCARET is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   MASCARET is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with MASCARET.  If not, see <http://www.gnu.org/licenses/>
!

module M_FH_I
! *********************************************************************
! PROGICIEL : MASCARET         N. GOUTAL
!
! VERSION : V8P2R0                EDF-CEREMA
! *********************************************************************

   interface

   function FH( &
         TIRANT &
               )

!***********************************************************************
!     CODE MASCARET : FONCTION UTILISEE POUR LE CALCUL DE LA SOLUTION
!                     ANALYTIQUE DANS LE SOUS-PROGRAMME SEUIL
!
!-----------------------------------------------------------------------

   !============================= Declarations ===========================

   !.. Modules importes ..
   !----------------------
   use M_PRECISION

   !.. Declarations explicites ..
   !-----------------------------
   implicit none

   !.. Argument ..
   !--------------
   real(DOUBLE)                                  :: FH
   real(DOUBLE),                   intent(in)    :: TIRANT

   !.. Communs ..
   !-------------
   integer ALPHA
   real(DOUBLE) A1,A2,A3
   common /COEFS/ ALPHA
   common /COEFH/ A1,A2,A3

   end function FH

   end interface

end module M_FH_I
