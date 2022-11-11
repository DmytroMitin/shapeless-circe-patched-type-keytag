
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

import function.FnToProduct

trait FnToProductInstances {
  type Aux[F, P] = FnToProduct[F] { type Out = P }

  implicit def fnToProduct0[
    Res
  ]: Aux[
    (() => Res),
    (HNil) => Res
  ] = FnToProduct.instance(fn => _ => fn())

  implicit def fnToProduct1[
    A, Res
  ]: Aux[
    ((A) => Res),
    (A::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::HNil => fn(a) })

  implicit def fnToProduct2[
    A, B, Res
  ]: Aux[
    ((A, B) => Res),
    (A::B::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::HNil => fn(a, b) })

  implicit def fnToProduct3[
    A, B, C, Res
  ]: Aux[
    ((A, B, C) => Res),
    (A::B::C::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::HNil => fn(a, b, c) })

  implicit def fnToProduct4[
    A, B, C, D, Res
  ]: Aux[
    ((A, B, C, D) => Res),
    (A::B::C::D::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::HNil => fn(a, b, c, d) })

  implicit def fnToProduct5[
    A, B, C, D, E, Res
  ]: Aux[
    ((A, B, C, D, E) => Res),
    (A::B::C::D::E::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::HNil => fn(a, b, c, d, e) })

  implicit def fnToProduct6[
    A, B, C, D, E, F, Res
  ]: Aux[
    ((A, B, C, D, E, F) => Res),
    (A::B::C::D::E::F::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::HNil => fn(a, b, c, d, e, f) })

  implicit def fnToProduct7[
    A, B, C, D, E, F, G, Res
  ]: Aux[
    ((A, B, C, D, E, F, G) => Res),
    (A::B::C::D::E::F::G::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::HNil => fn(a, b, c, d, e, f, g) })

  implicit def fnToProduct8[
    A, B, C, D, E, F, G, H, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H) => Res),
    (A::B::C::D::E::F::G::H::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::HNil => fn(a, b, c, d, e, f, g, h) })

  implicit def fnToProduct9[
    A, B, C, D, E, F, G, H, I, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I) => Res),
    (A::B::C::D::E::F::G::H::I::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::HNil => fn(a, b, c, d, e, f, g, h, i) })

  implicit def fnToProduct10[
    A, B, C, D, E, F, G, H, I, J, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J) => Res),
    (A::B::C::D::E::F::G::H::I::J::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::HNil => fn(a, b, c, d, e, f, g, h, i, j) })

  implicit def fnToProduct11[
    A, B, C, D, E, F, G, H, I, J, K, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::HNil => fn(a, b, c, d, e, f, g, h, i, j, k) })

  implicit def fnToProduct12[
    A, B, C, D, E, F, G, H, I, J, K, L, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l) })

  implicit def fnToProduct13[
    A, B, C, D, E, F, G, H, I, J, K, L, M, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m) })

  implicit def fnToProduct14[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n) })

  implicit def fnToProduct15[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) })

  implicit def fnToProduct16[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) })

  implicit def fnToProduct17[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) })

  implicit def fnToProduct18[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) })

  implicit def fnToProduct19[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) })

  implicit def fnToProduct20[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) })

  implicit def fnToProduct21[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) })

  implicit def fnToProduct22[
    A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Res
  ]: Aux[
    ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Res),
    (A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::V::HNil) => Res
  ] = FnToProduct.instance(fn => { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::v::HNil => fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) })
}