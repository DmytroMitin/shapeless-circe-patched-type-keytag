
/*
 * Copyright (c) 2011-18 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless
  
trait TupleTypeableInstances {
  import syntax.typeable._

  implicit def tuple1Typeable[
    A
  ](
    implicit castA:Typeable[A]
  ): Typeable[Tuple1[A]] =
    Typeable.instance(s"(${castA.describe})") {
      case p: Tuple1[_] =>
        for (_ <- p._1.cast[A])
          yield p.asInstanceOf[Tuple1[A]]
      case _ =>
        None
    }

  implicit def tuple2Typeable[
    A, B
  ](
    implicit castA:Typeable[A], castB:Typeable[B]
  ): Typeable[(A, B)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe})") {
      case p: (_, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B])
          yield p.asInstanceOf[(A, B)]
      case _ =>
        None
    }

  implicit def tuple3Typeable[
    A, B, C
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C]
  ): Typeable[(A, B, C)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe})") {
      case p: (_, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C])
          yield p.asInstanceOf[(A, B, C)]
      case _ =>
        None
    }

  implicit def tuple4Typeable[
    A, B, C, D
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D]
  ): Typeable[(A, B, C, D)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe})") {
      case p: (_, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D])
          yield p.asInstanceOf[(A, B, C, D)]
      case _ =>
        None
    }

  implicit def tuple5Typeable[
    A, B, C, D, E
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E]
  ): Typeable[(A, B, C, D, E)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe})") {
      case p: (_, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E])
          yield p.asInstanceOf[(A, B, C, D, E)]
      case _ =>
        None
    }

  implicit def tuple6Typeable[
    A, B, C, D, E, F
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F]
  ): Typeable[(A, B, C, D, E, F)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe})") {
      case p: (_, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F])
          yield p.asInstanceOf[(A, B, C, D, E, F)]
      case _ =>
        None
    }

  implicit def tuple7Typeable[
    A, B, C, D, E, F, G
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G]
  ): Typeable[(A, B, C, D, E, F, G)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe})") {
      case p: (_, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G])
          yield p.asInstanceOf[(A, B, C, D, E, F, G)]
      case _ =>
        None
    }

  implicit def tuple8Typeable[
    A, B, C, D, E, F, G, H
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H]
  ): Typeable[(A, B, C, D, E, F, G, H)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe})") {
      case p: (_, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H)]
      case _ =>
        None
    }

  implicit def tuple9Typeable[
    A, B, C, D, E, F, G, H, I
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I]
  ): Typeable[(A, B, C, D, E, F, G, H, I)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe})") {
      case p: (_, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I)]
      case _ =>
        None
    }

  implicit def tuple10Typeable[
    A, B, C, D, E, F, G, H, I, J
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J)]
      case _ =>
        None
    }

  implicit def tuple11Typeable[
    A, B, C, D, E, F, G, H, I, J, K
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K)]
      case _ =>
        None
    }

  implicit def tuple12Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L)]
      case _ =>
        None
    }

  implicit def tuple13Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
      case _ =>
        None
    }

  implicit def tuple14Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
      case _ =>
        None
    }

  implicit def tuple15Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
      case _ =>
        None
    }

  implicit def tuple16Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
      case _ =>
        None
    }

  implicit def tuple17Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
      case _ =>
        None
    }

  implicit def tuple18Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q], castR:Typeable[R]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe}, ${castR.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q]; _ <- p._18.cast[R])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
      case _ =>
        None
    }

  implicit def tuple19Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q], castR:Typeable[R], castS:Typeable[S]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe}, ${castR.describe}, ${castS.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q]; _ <- p._18.cast[R]; _ <- p._19.cast[S])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
      case _ =>
        None
    }

  implicit def tuple20Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q], castR:Typeable[R], castS:Typeable[S], castT:Typeable[T]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe}, ${castR.describe}, ${castS.describe}, ${castT.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q]; _ <- p._18.cast[R]; _ <- p._19.cast[S]; _ <- p._20.cast[T])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
      case _ =>
        None
    }

  implicit def tuple21Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q], castR:Typeable[R], castS:Typeable[S], castT:Typeable[T], castU:Typeable[U]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe}, ${castR.describe}, ${castS.describe}, ${castT.describe}, ${castU.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q]; _ <- p._18.cast[R]; _ <- p._19.cast[S]; _ <- p._20.cast[T]; _ <- p._21.cast[U])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
      case _ =>
        None
    }

  implicit def tuple22Typeable[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
  ](
    implicit castA:Typeable[A], castB:Typeable[B], castC:Typeable[C], castD:Typeable[D], castE:Typeable[E], castF:Typeable[F], castG:Typeable[G], castH:Typeable[H], castI:Typeable[I], castJ:Typeable[J], castK:Typeable[K], castL:Typeable[L], castM:Typeable[M], castN:Typeable[N], castO:Typeable[O], castP:Typeable[P], castQ:Typeable[Q], castR:Typeable[R], castS:Typeable[S], castT:Typeable[T], castU:Typeable[U], castV:Typeable[V]
  ): Typeable[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    Typeable.instance(s"(${castA.describe}, ${castB.describe}, ${castC.describe}, ${castD.describe}, ${castE.describe}, ${castF.describe}, ${castG.describe}, ${castH.describe}, ${castI.describe}, ${castJ.describe}, ${castK.describe}, ${castL.describe}, ${castM.describe}, ${castN.describe}, ${castO.describe}, ${castP.describe}, ${castQ.describe}, ${castR.describe}, ${castS.describe}, ${castT.describe}, ${castU.describe}, ${castV.describe})") {
      case p: (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        for (_ <- p._1.cast[A]; _ <- p._2.cast[B]; _ <- p._3.cast[C]; _ <- p._4.cast[D]; _ <- p._5.cast[E]; _ <- p._6.cast[F]; _ <- p._7.cast[G]; _ <- p._8.cast[H]; _ <- p._9.cast[I]; _ <- p._10.cast[J]; _ <- p._11.cast[K]; _ <- p._12.cast[L]; _ <- p._13.cast[M]; _ <- p._14.cast[N]; _ <- p._15.cast[O]; _ <- p._16.cast[P]; _ <- p._17.cast[Q]; _ <- p._18.cast[R]; _ <- p._19.cast[S]; _ <- p._20.cast[T]; _ <- p._21.cast[U]; _ <- p._22.cast[V])
          yield p.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
      case _ =>
        None
    }
}