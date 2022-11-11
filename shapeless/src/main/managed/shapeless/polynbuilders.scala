
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
  

/**
  * Provides elegant syntax for creating polys from functions
  *
  * @author Aristotelis Dossas
  */
object PolyNBuilders {

 trait Poly1Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A] {
     def apply[Out](λ: (A) => Out) = {
       new Poly1Builder[((A) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A] = new AtAux[A]

   def build = new Poly1 {
     val functions = self.functions

     implicit def allCases[A, Out](implicit tL: Function1TypeAt[A, Out, HL]) = {
       val func: (A) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly1Builder */
 trait Function1TypeAt[A, Out, HL <: HList] {
   def apply(l: HL): (A) => Out
 }

 object Function1TypeAt {
   private def instance[A, Out, HL <: HList](
     f: HL => (A) => Out
   ): Function1TypeAt[A, Out, HL] =
     new Function1TypeAt[A, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, Out, Tail <: HList
   ]: Function1TypeAt[A, Out, ((A) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, Out, Tail <: HList, Head
   ](
     implicit tprev: Function1TypeAt[A, Out, Tail]
   ): Function1TypeAt[A, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly2Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B] {
     def apply[Out](λ: (A, B) => Out) = {
       new Poly2Builder[((A, B) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B] = new AtAux[A, B]

   def build = new Poly2 {
     val functions = self.functions

     implicit def allCases[A, B, Out](implicit tL: Function2TypeAt[A, B, Out, HL]) = {
       val func: (A, B) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly2Builder */
 trait Function2TypeAt[A, B, Out, HL <: HList] {
   def apply(l: HL): (A, B) => Out
 }

 object Function2TypeAt {
   private def instance[A, B, Out, HL <: HList](
     f: HL => (A, B) => Out
   ): Function2TypeAt[A, B, Out, HL] =
     new Function2TypeAt[A, B, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, Out, Tail <: HList
   ]: Function2TypeAt[A, B, Out, ((A, B) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, Out, Tail <: HList, Head
   ](
     implicit tprev: Function2TypeAt[A, B, Out, Tail]
   ): Function2TypeAt[A, B, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly3Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C] {
     def apply[Out](λ: (A, B, C) => Out) = {
       new Poly3Builder[((A, B, C) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C] = new AtAux[A, B, C]

   def build = new Poly3 {
     val functions = self.functions

     implicit def allCases[A, B, C, Out](implicit tL: Function3TypeAt[A, B, C, Out, HL]) = {
       val func: (A, B, C) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly3Builder */
 trait Function3TypeAt[A, B, C, Out, HL <: HList] {
   def apply(l: HL): (A, B, C) => Out
 }

 object Function3TypeAt {
   private def instance[A, B, C, Out, HL <: HList](
     f: HL => (A, B, C) => Out
   ): Function3TypeAt[A, B, C, Out, HL] =
     new Function3TypeAt[A, B, C, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, Out, Tail <: HList
   ]: Function3TypeAt[A, B, C, Out, ((A, B, C) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, Out, Tail <: HList, Head
   ](
     implicit tprev: Function3TypeAt[A, B, C, Out, Tail]
   ): Function3TypeAt[A, B, C, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly4Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D] {
     def apply[Out](λ: (A, B, C, D) => Out) = {
       new Poly4Builder[((A, B, C, D) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D] = new AtAux[A, B, C, D]

   def build = new Poly4 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, Out](implicit tL: Function4TypeAt[A, B, C, D, Out, HL]) = {
       val func: (A, B, C, D) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly4Builder */
 trait Function4TypeAt[A, B, C, D, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D) => Out
 }

 object Function4TypeAt {
   private def instance[A, B, C, D, Out, HL <: HList](
     f: HL => (A, B, C, D) => Out
   ): Function4TypeAt[A, B, C, D, Out, HL] =
     new Function4TypeAt[A, B, C, D, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, Out, Tail <: HList
   ]: Function4TypeAt[A, B, C, D, Out, ((A, B, C, D) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, Out, Tail <: HList, Head
   ](
     implicit tprev: Function4TypeAt[A, B, C, D, Out, Tail]
   ): Function4TypeAt[A, B, C, D, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly5Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E] {
     def apply[Out](λ: (A, B, C, D, E) => Out) = {
       new Poly5Builder[((A, B, C, D, E) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E] = new AtAux[A, B, C, D, E]

   def build = new Poly5 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, Out](implicit tL: Function5TypeAt[A, B, C, D, E, Out, HL]) = {
       val func: (A, B, C, D, E) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly5Builder */
 trait Function5TypeAt[A, B, C, D, E, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E) => Out
 }

 object Function5TypeAt {
   private def instance[A, B, C, D, E, Out, HL <: HList](
     f: HL => (A, B, C, D, E) => Out
   ): Function5TypeAt[A, B, C, D, E, Out, HL] =
     new Function5TypeAt[A, B, C, D, E, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, Out, Tail <: HList
   ]: Function5TypeAt[A, B, C, D, E, Out, ((A, B, C, D, E) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, Out, Tail <: HList, Head
   ](
     implicit tprev: Function5TypeAt[A, B, C, D, E, Out, Tail]
   ): Function5TypeAt[A, B, C, D, E, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly6Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F] {
     def apply[Out](λ: (A, B, C, D, E, F) => Out) = {
       new Poly6Builder[((A, B, C, D, E, F) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F] = new AtAux[A, B, C, D, E, F]

   def build = new Poly6 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, Out](implicit tL: Function6TypeAt[A, B, C, D, E, F, Out, HL]) = {
       val func: (A, B, C, D, E, F) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly6Builder */
 trait Function6TypeAt[A, B, C, D, E, F, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F) => Out
 }

 object Function6TypeAt {
   private def instance[A, B, C, D, E, F, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F) => Out
   ): Function6TypeAt[A, B, C, D, E, F, Out, HL] =
     new Function6TypeAt[A, B, C, D, E, F, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, Out, Tail <: HList
   ]: Function6TypeAt[A, B, C, D, E, F, Out, ((A, B, C, D, E, F) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, Out, Tail <: HList, Head
   ](
     implicit tprev: Function6TypeAt[A, B, C, D, E, F, Out, Tail]
   ): Function6TypeAt[A, B, C, D, E, F, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly7Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G] {
     def apply[Out](λ: (A, B, C, D, E, F, G) => Out) = {
       new Poly7Builder[((A, B, C, D, E, F, G) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G] = new AtAux[A, B, C, D, E, F, G]

   def build = new Poly7 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, Out](implicit tL: Function7TypeAt[A, B, C, D, E, F, G, Out, HL]) = {
       val func: (A, B, C, D, E, F, G) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly7Builder */
 trait Function7TypeAt[A, B, C, D, E, F, G, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G) => Out
 }

 object Function7TypeAt {
   private def instance[A, B, C, D, E, F, G, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G) => Out
   ): Function7TypeAt[A, B, C, D, E, F, G, Out, HL] =
     new Function7TypeAt[A, B, C, D, E, F, G, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, Out, Tail <: HList
   ]: Function7TypeAt[A, B, C, D, E, F, G, Out, ((A, B, C, D, E, F, G) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, Out, Tail <: HList, Head
   ](
     implicit tprev: Function7TypeAt[A, B, C, D, E, F, G, Out, Tail]
   ): Function7TypeAt[A, B, C, D, E, F, G, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly8Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H) => Out) = {
       new Poly8Builder[((A, B, C, D, E, F, G, H) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H] = new AtAux[A, B, C, D, E, F, G, H]

   def build = new Poly8 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, Out](implicit tL: Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly8Builder */
 trait Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H) => Out
 }

 object Function8TypeAt {
   private def instance[A, B, C, D, E, F, G, H, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H) => Out
   ): Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL] =
     new Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, Out, Tail <: HList
   ]: Function8TypeAt[A, B, C, D, E, F, G, H, Out, ((A, B, C, D, E, F, G, H) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, Out, Tail <: HList, Head
   ](
     implicit tprev: Function8TypeAt[A, B, C, D, E, F, G, H, Out, Tail]
   ): Function8TypeAt[A, B, C, D, E, F, G, H, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly9Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I) => Out) = {
       new Poly9Builder[((A, B, C, D, E, F, G, H, I) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I] = new AtAux[A, B, C, D, E, F, G, H, I]

   def build = new Poly9 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, Out](implicit tL: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly9Builder */
 trait Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I) => Out
 }

 object Function9TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I) => Out
   ): Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL] =
     new Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, Out, Tail <: HList
   ]: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, ((A, B, C, D, E, F, G, H, I) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, Out, Tail <: HList, Head
   ](
     implicit tprev: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, Tail]
   ): Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly10Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J) => Out) = {
       new Poly10Builder[((A, B, C, D, E, F, G, H, I, J) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J] = new AtAux[A, B, C, D, E, F, G, H, I, J]

   def build = new Poly10 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, Out](implicit tL: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly10Builder */
 trait Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J) => Out
 }

 object Function10TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J) => Out
   ): Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL] =
     new Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, Out, Tail <: HList
   ]: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, ((A, B, C, D, E, F, G, H, I, J) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, Out, Tail <: HList, Head
   ](
     implicit tprev: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, Tail]
   ): Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly11Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K) => Out) = {
       new Poly11Builder[((A, B, C, D, E, F, G, H, I, J, K) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K] = new AtAux[A, B, C, D, E, F, G, H, I, J, K]

   def build = new Poly11 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, Out](implicit tL: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly11Builder */
 trait Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K) => Out
 }

 object Function11TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K) => Out
   ): Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL] =
     new Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, Out, Tail <: HList
   ]: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, ((A, B, C, D, E, F, G, H, I, J, K) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, Out, Tail <: HList, Head
   ](
     implicit tprev: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, Tail]
   ): Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly12Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L) => Out) = {
       new Poly12Builder[((A, B, C, D, E, F, G, H, I, J, K, L) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L]

   def build = new Poly12 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, Out](implicit tL: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly12Builder */
 trait Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L) => Out
 }

 object Function12TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L) => Out
   ): Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL] =
     new Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail <: HList
   ]: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, ((A, B, C, D, E, F, G, H, I, J, K, L) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail <: HList, Head
   ](
     implicit tprev: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail]
   ): Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly13Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out) = {
       new Poly13Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M]

   def build = new Poly13 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, Out](implicit tL: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly13Builder */
 trait Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out
 }

 object Function13TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out
   ): Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL] =
     new Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail <: HList
   ]: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail <: HList, Head
   ](
     implicit tprev: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail]
   ): Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly14Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out) = {
       new Poly14Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N]

   def build = new Poly14 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out](implicit tL: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly14Builder */
 trait Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out
 }

 object Function14TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out
   ): Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL] =
     new Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail <: HList
   ]: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail <: HList, Head
   ](
     implicit tprev: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail]
   ): Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly15Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out) = {
       new Poly15Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

   def build = new Poly15 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out](implicit tL: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly15Builder */
 trait Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out
 }

 object Function15TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out
   ): Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL] =
     new Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail <: HList
   ]: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail <: HList, Head
   ](
     implicit tprev: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail]
   ): Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly16Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out) = {
       new Poly16Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]

   def build = new Poly16 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out](implicit tL: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly16Builder */
 trait Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out
 }

 object Function16TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out
   ): Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL] =
     new Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail <: HList
   ]: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail <: HList, Head
   ](
     implicit tprev: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail]
   ): Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly17Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out) = {
       new Poly17Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]

   def build = new Poly17 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out](implicit tL: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly17Builder */
 trait Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out
 }

 object Function17TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out
   ): Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL] =
     new Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail <: HList
   ]: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail <: HList, Head
   ](
     implicit tprev: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail]
   ): Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly18Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out) = {
       new Poly18Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]

   def build = new Poly18 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out](implicit tL: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly18Builder */
 trait Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out
 }

 object Function18TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out
   ): Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL] =
     new Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail <: HList
   ]: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail <: HList, Head
   ](
     implicit tprev: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail]
   ): Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly19Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out) = {
       new Poly19Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]

   def build = new Poly19 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out](implicit tL: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly19Builder */
 trait Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out
 }

 object Function19TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out
   ): Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL] =
     new Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail <: HList
   ]: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail <: HList, Head
   ](
     implicit tprev: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail]
   ): Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly20Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out) = {
       new Poly20Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]

   def build = new Poly20 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out](implicit tL: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly20Builder */
 trait Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out
 }

 object Function20TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out
   ): Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL] =
     new Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail <: HList
   ]: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail <: HList, Head
   ](
     implicit tprev: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail]
   ): Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly21Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out) = {
       new Poly21Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]

   def build = new Poly21 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out](implicit tL: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly21Builder */
 trait Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out
 }

 object Function21TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out
   ): Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL] =
     new Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail <: HList
   ]: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail <: HList, Head
   ](
     implicit tprev: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail]
   ): Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }

 trait Poly22Builder[HL <: HList] { self =>

   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out) = {
       new Poly22Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }
   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]

   def build = new Poly22 {
     val functions = self.functions

     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out](implicit tL: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL]) = {
       val func: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out = tL(functions)
       at(func)
     }
   }
 }

 /* For internal use of Poly22Builder */
 trait Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out
 }

 object Function22TypeAt {
   private def instance[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL <: HList](
     f: HL => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out
   ): Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL] =
     new Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL] {
       def apply(l: HL) = f(l)
     }

   implicit def at0[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail <: HList
   ]: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out) :: Tail] =
     instance(_.head)

   implicit def atOther[
     A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail <: HList, Head
   ](
     implicit tprev: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail]
   ): Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Head :: Tail] =
     instance(l => tprev(l.tail))
 }
}