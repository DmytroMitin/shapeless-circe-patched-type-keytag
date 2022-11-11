
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

import hlist.Tupler

trait TuplerInstances {
  type Aux[L <: HList, T] = Tupler[L] { type Out = T }

  implicit def hlistTupler1[
    A
  ]: Aux[
    A::HNil,
    Tuple1[A]
  ] = Tupler.instance { case a::HNil =>
    Tuple1(a)
  }

  implicit def hlistTupler2[
    A, B
  ]: Aux[
    A::B::HNil,
    (A, B)
  ] = Tupler.instance { case a::b::HNil =>
    (a, b)
  }

  implicit def hlistTupler3[
    A, B, C
  ]: Aux[
    A::B::C::HNil,
    (A, B, C)
  ] = Tupler.instance { case a::b::c::HNil =>
    (a, b, c)
  }

  implicit def hlistTupler4[
    A, B, C, D
  ]: Aux[
    A::B::C::D::HNil,
    (A, B, C, D)
  ] = Tupler.instance { case a::b::c::d::HNil =>
    (a, b, c, d)
  }

  implicit def hlistTupler5[
    A, B, C, D, E
  ]: Aux[
    A::B::C::D::E::HNil,
    (A, B, C, D, E)
  ] = Tupler.instance { case a::b::c::d::e::HNil =>
    (a, b, c, d, e)
  }

  implicit def hlistTupler6[
    A, B, C, D, E, F
  ]: Aux[
    A::B::C::D::E::F::HNil,
    (A, B, C, D, E, F)
  ] = Tupler.instance { case a::b::c::d::e::f::HNil =>
    (a, b, c, d, e, f)
  }

  implicit def hlistTupler7[
    A, B, C, D, E, F, G
  ]: Aux[
    A::B::C::D::E::F::G::HNil,
    (A, B, C, D, E, F, G)
  ] = Tupler.instance { case a::b::c::d::e::f::g::HNil =>
    (a, b, c, d, e, f, g)
  }

  implicit def hlistTupler8[
    A, B, C, D, E, F, G, H
  ]: Aux[
    A::B::C::D::E::F::G::H::HNil,
    (A, B, C, D, E, F, G, H)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::HNil =>
    (a, b, c, d, e, f, g, h)
  }

  implicit def hlistTupler9[
    A, B, C, D, E, F, G, H, I
  ]: Aux[
    A::B::C::D::E::F::G::H::I::HNil,
    (A, B, C, D, E, F, G, H, I)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::HNil =>
    (a, b, c, d, e, f, g, h, i)
  }

  implicit def hlistTupler10[
    A, B, C, D, E, F, G, H, I, J
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::HNil,
    (A, B, C, D, E, F, G, H, I, J)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::HNil =>
    (a, b, c, d, e, f, g, h, i, j)
  }

  implicit def hlistTupler11[
    A, B, C, D, E, F, G, H, I, J, K
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::HNil,
    (A, B, C, D, E, F, G, H, I, J, K)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k)
  }

  implicit def hlistTupler12[
    A, B, C, D, E, F, G, H, I, J, K, L
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l)
  }

  implicit def hlistTupler13[
    A, B, C, D, E, F, G, H, I, J, K, L, M
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m)
  }

  implicit def hlistTupler14[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  }

  implicit def hlistTupler15[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  }

  implicit def hlistTupler16[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  }

  implicit def hlistTupler17[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  }

  implicit def hlistTupler18[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  }

  implicit def hlistTupler19[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  }

  implicit def hlistTupler20[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  }

  implicit def hlistTupler21[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  }

  implicit def hlistTupler22[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
  ]: Aux[
    A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::V::HNil,
    (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
  ] = Tupler.instance { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::v::HNil =>
    (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  }
}