#ifndef VCTRS_VCTRS_SLICING_INDEX__H
#define VCTRS_VCTRS_SLICING_INDEX__H

namespace vctrs {

// A SlicingIndex allows specifying which rows of a data frame are selected in which order, basically a 0:n -> 0:m map.
// It also can be used to split a data frame in groups.
// Important special cases can be implemented without materializing the map.
  class SlicingIndex {
  public:
    virtual size_t size() const = 0;

    virtual size_t operator[](size_t i) const = 0;

    virtual int group() const = 0;

    virtual bool is_tight(size_t length, size_t offset = 0) const {
      return FALSE;
    };
  };

// A GroupedSlicingIndex is the most general slicing index,
// the 0:n -> 0:m map is specified and stored as an IntegerVector.
// A group identifier can be assigned on construction.
// It is used in grouped operations (group_by()).
  class GroupedSlicingIndex : public SlicingIndex {
  public:
    GroupedSlicingIndex(Rcpp::IntegerVector data_) : data(data_), group_index(-1) {
    }

    GroupedSlicingIndex(Rcpp::IntegerVector data_, int group_) : data(data_), group_index(group_) {
    }

    virtual size_t size() const {
      return data.size();
    }

    virtual size_t operator[](size_t i) const {
      return data[i];
    }

    virtual int group() const {
      return group_index;
    }

  private:
    Rcpp::IntegerVector data;
    int group_index;
  };

// A RowwiseSlicingIndex selects a single row, which is also the group ID by definition.
// It is used in rowwise operations (rowwise()).
  class RowwiseSlicingIndex : public SlicingIndex {
  public:
    RowwiseSlicingIndex(const int start_) : start(start_) {
    }

    inline size_t size() const {
      return 1;
    }

    inline size_t operator[](size_t i) const {
      if (i != 0)
        Rcpp::stop("Can only use 0 for RowwiseSlicingIndex, queried %d", i);
      return start;
    }

    inline int group() const {
      return start;
    }

  private:
    int start;
  };

// A NaturalSlicingIndex selects an entire data frame as a single group.
// It is used when the entire data frame needs to be processed by a processor that expects a SlicingIndex
// to address the rows.
  class NaturalSlicingIndex : public SlicingIndex {
  public:
    NaturalSlicingIndex(const size_t n_) : n(n_) {
    }

    virtual size_t size() const {
      return n;
    }

    virtual size_t operator[](size_t i) const {
      if (i >= n)
        Rcpp::stop("Out of bounds index %d queried for NaturalSlicingIndex", i);
      return i;
    }

    virtual int group() const {
      return -1;
    }

    virtual bool is_tight(size_t length, size_t offset = 0) const {
      return offset == 0 && length == n;
    }

  private:
    size_t n;
  };

// An OffsetSlicingIndex selects a consecutive part of a data frame, starting at a specific row.
// It is used for binding data frames vertically (bind_rows()).
  class OffsetSlicingIndex : public SlicingIndex {
  public:
    OffsetSlicingIndex(const size_t start_, const size_t n_) : start(start_), n(n_) {
    }

    inline size_t size() const {
      return n;
    }

    inline size_t operator[](size_t i) const {
      if (i >= n)
        Rcpp::stop("Out of bounds index %d queried for OffsetSlicingIndex", i);
      return i + start;
    }

    inline int group() const {
      return -1;
    }

    virtual bool is_tight(size_t length, size_t offset = 0) const {
      return offset == start && length == n;
    }

  private:
    size_t start, n;
  };

}

#endif // VCTRS_VCTRS_SLICING_INDEX__H
