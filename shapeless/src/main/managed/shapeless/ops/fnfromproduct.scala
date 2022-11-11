
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
  package ops

import function.FnFromProduct

trait FnFromProductInstances {
  type Aux[F, O] = FnFromProduct[F] { type Out = O }

  implicit def fnFromProduct0[
    Res
  ]: Aux[
    (HNil) => Res,
    () => Res
  ] = FnFromProduct.instance { hf =>
    () =>
      hf(HNil)
  }

  implicit def fnFromProduct1[
    A, Res
  ]: Aux[
    (A::HNil) => Res,
    (A) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A) =>
      hf(a::HNil)
  }

  implicit def fnFromProduct2[
    A, B, Res
  ]: Aux[
    (A::B::HNil) => Res,
    (A, B) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B) =>
      hf(a::b::HNil)
  }

  implicit def fnFromProduct3[
    A, B, C, Res
  ]: Aux[
    (A::B::C::HNil) => Res,
    (A, B, C) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C) =>
      hf(a::b::c::HNil)
  }

  implicit def fnFromProduct4[
    A, B, C, D, Res
  ]: Aux[
    (A::B::C::D::HNil) => Res,
    (A, B, C, D) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D) =>
      hf(a::b::c::d::HNil)
  }

  implicit def fnFromProduct5[
    A, B, C, D, E, Res
  ]: Aux[
    (A::B::C::D::E::HNil) => Res,
    (A, B, C, D, E) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E) =>
      hf(a::b::c::d::e::HNil)
  }

  implicit def fnFromProduct6[
    A, B, C, D, E, F, Res
  ]: Aux[
    (A::B::C::D::E::F::HNil) => Res,
    (A, B, C, D, E, F) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F) =>
      hf(a::b::c::d::e::f::HNil)
  }

  implicit def fnFromProduct7[
    A, B, C, D, E, F, G, Res
  ]: Aux[
    (A::B::C::D::E::F::G::HNil) => Res,
    (A, B, C, D, E, F, G) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G) =>
      hf(a::b::c::d::e::f::g::HNil)
  }

  implicit def fnFromProduct8[
    A, B, C, D, E, F, G, H, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::HNil) => Res,
    (A, B, C, D, E, F, G, H) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H) =>
      hf(a::b::c::d::e::f::g::h::HNil)
  }

  implicit def fnFromProduct9[
    A, B, C, D, E, F, G, H, I, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::HNil) => Res,
    (A, B, C, D, E, F, G, H, I) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I) =>
      hf(a::b::c::d::e::f::g::h::i::HNil)
  }

  implicit def fnFromProduct10[
    A, B, C, D, E, F, G, H, I, J, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J) =>
      hf(a::b::c::d::e::f::g::h::i::j::HNil)
  }

  implicit def fnFromProduct11[
    A, B, C, D, E, F, G, H, I, J, K, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::HNil)
  }

  implicit def fnFromProduct12[
    A, B, C, D, E, F, G, H, I, J, K, L, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::HNil)
  }

  implicit def fnFromProduct13[
    A, B, C, D, E, F, G, H, I, J, K, L, M, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::HNil)
  }

  implicit def fnFromProduct14[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::HNil)
  }

  implicit def fnFromProduct15[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::HNil)
  }

  implicit def fnFromProduct16[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::HNil)
  }

  implicit def fnFromProduct17[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::HNil)
  }

  implicit def fnFromProduct18[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q, r:R) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::HNil)
  }

  implicit def fnFromProduct19[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q, r:R, s:S) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::HNil)
  }

  implicit def fnFromProduct20[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q, r:R, s:S, t:T) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::HNil)
  }

  implicit def fnFromProduct21[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q, r:R, s:S, t:T, u:U) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::HNil)
  }

  implicit def fnFromProduct22[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Res
  ]: Aux[
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::V::HNil) => Res,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Res
  ] = FnFromProduct.instance { hf =>
    (a:A, b:B, c:C, d:D, e:E, f:F, g:G, h:H, i:I, j:J, k:K, l:L, m:M, n:N, o:O, p:P, q:Q, r:R, s:S, t:T, u:U, v:V) =>
      hf(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::v::HNil)
  }
}