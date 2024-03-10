/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   Copyright (c) 2011-2023 The plumed team
   (see the PEOPLE file at the root of the distribution for a list of names)

   See http://www.plumed.org for more information.

   This file is part of plumed, version 2.

   plumed is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   plumed is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with plumed.  If not, see <http://www.gnu.org/licenses/>.
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
#ifndef __PLUMED_tools_MergeVectorTools_h
#define __PLUMED_tools_MergeVectorTools_h

#include "small_vector/small_vector.h"
#include <vector>
#include <algorithm>

#include <iostream>

namespace PLMD {

namespace mergeVectorTools {

  namespace detail {

    template<class C,class Entry>
    static void mergeSortedVectorsImpl(const C* const* vecs, std::size_t size, std::vector<typename C::value_type> & result) {

      // heap containing the ranges to be pushed
      // avoid allocations when it's small
      gch::small_vector<Entry,32> heap;

      for(unsigned i=0; i<size; i++) {
        if(!vecs[i]->empty()) {
          // add entry at the end of the array
          heap.emplace_back(Entry(*vecs[i]));
        }
      }
      // make it a sorted heap
      std::make_heap(std::begin(heap),std::end(heap));

      // first iteration, to avoid an extra "if" in main iteration
      {
        // move entry with the smallest element to the back of the array
        std::pop_heap(std::begin(heap), std::end(heap));
        // entry
        auto & tmp=heap.back();
        // move forward the used entry and get previous value
        // element
        const auto val=tmp.next();
        // this is the first iteration, so we do not check result.back() value
        // and directly push this element
        result.push_back(val);
        // if this entry is exhausted, remove it from the array
        if(tmp.empty()) heap.pop_back();
        // otherwise, sort again the heap
        else std::push_heap(std::begin(heap), std::end(heap));
      }

      while(!heap.empty()) {
        // move entry with the smallest element to the back of the array
        std::pop_heap(std::begin(heap), std::end(heap));
        // entry
        auto & tmp=heap.back();
        // element
        const auto val=tmp.next();
        // if the element is larger than the current largest element,
        // push it to result
        if(val > result.back()) result.push_back(val);
        // if this entry is exhausted, remove it from the array
        if(tmp.empty()) heap.pop_back();
        // otherwise, sort again the heap
        else std::push_heap(std::begin(heap), std::end(heap));
      }
    }

    /// utility class storing the range of remaining objects to be pushed
    template<class C,typename NElem>
    class Entry
    {
      // Since vector.size() is size_t, there could be more than 2^32 elements.
      // This is currently impossible with AtomNumer, but the code is general
      // in case we use this tool in other contexts
      // For the standard settings, the size of this object is 8+4+4=16 bytes
      // which should guarantee fast and aligned access.

      typename C::const_iterator fwdIt; // 8 bytes (pointer)
      NElem nelem; // 4 or 8 bytes
      typename C::value_type next_elem; // 4 bytes for AtomNumber

    public:
      template<class V>
      explicit Entry(V const& v) :
        fwdIt(v.begin()), nelem(v.size()) {
        if(v.data()) next_elem=v[0];
      }
      /// check if this vector still contains something to be pushed
      bool empty() const { return nelem == 0; }
      /// to allow using a max heap, which selects the highest element.
      /// we here (counterintuitively) define < as >
      bool operator< (Entry const& rhs) const { return top() > rhs.top(); }
      /// get the value of the smallest element in this entry
      auto top() const { return next_elem; }
      /// advance this entry
      auto next() {
        auto result=*fwdIt;
        fwdIt++;
        nelem--;
        if(nelem!=0) next_elem=*fwdIt;
        return result;
      };
    };

  } // detail

  /// Merge sorted vectors.
  /// Takes a vector of pointers to containers and merge them.
  /// Containers should be already sorted.
  /// The content is appended to the result vector.
  template<class C>
  static void mergeSortedVectors(const C* const* vecs, std::size_t size, std::vector<typename C::value_type> & result) {

    // preprocessing
    std::size_t maxsize=0;

    for(unsigned i=0; i<size; i++) {
      // find the largest
      maxsize=std::max(maxsize,vecs[i]->size());
    }
    // if vectors are empty we are done
    if(maxsize==0) return;

    // this is to decrease the number of reallocations on push_back
    result.reserve(result.size()+maxsize);

    // this is here for future extensibility
    // with the current use, a pre-sorted vector of AtomNumber cannot
    // contain more than unsigned::max() elements, so that
    // only the first implementation will be used
    if(maxsize<=std::numeric_limits<unsigned>::max()) {
      detail::mergeSortedVectorsImpl<C,detail::Entry<C,unsigned>>(vecs, size, result);
    } else {
      detail::mergeSortedVectorsImpl<C,detail::Entry<C,std::size_t>>(vecs, size, result);
    }
  }

  /// Utility function taking a std::vector argument rather than a span-like pair of arguments
  template<class C>
  static void mergeSortedVectors(const std::vector<C*> & vecs, std::vector<typename C::value_type> & result) {
    mergeSortedVectors(vecs.data(),vecs.size(),result);
  }
}

}

#endif

