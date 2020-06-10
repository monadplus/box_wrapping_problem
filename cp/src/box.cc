#include <cassert>
#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
#include <fstream>
#include <vector>

using namespace std;
using namespace Gecode;

bool are_equal(const pair<int,int>& b1, const pair<int,int>& b2) {
  return b1.first == b2.first && b1.second == b2.second;
}

bool cmp(const pair<int,int>& b1, const pair<int,int>& b2) {
  return (b1.first * b1.second) > (b2.first * b2.second);
}

int upper_bound_length(int w, int m, int width, int height) {
  int boxes_row = w / min(width,height);
  int min_rows = std::ceil(m / ((double)boxes_row));
  return min_rows * std::max(width,height);
}

class Box : public Space {

  private:
    int w;
    vector<pair<int,int>> boxes;

  protected:
    IntVar length;

    IntVarArray x_top;
    IntVarArray y_top;

    // FIXME
    // We  can remove those variables but it is not as simple as in the LP problem.
    // Because we need to reformulate the rotation.
    IntVarArray x_bottom;
    IntVarArray y_bottom;

  public:

    int value_aux(IntVar x, int i) const {
      if(i == 0) return x.min();

      int dim = max(boxes[i-1].first, boxes[i-1].second);
      if (dim > w)
        dim = min(boxes[i-1].first, boxes[i-1].second);

      int x_top_ant = x_top[i-1].val() + dim;

      int val = x.min();
      for (IntVarValues v(x); v( ) ; ++v) {
        if(v.val() >= x_top_ant) {
           val = v.val();
           break;
        }
      }
      return val;
    }

    static int value(const Space& home, IntVar x, int i) {
      return static_cast<const Box&>(home).value_aux(x,i);
    }

    static void commit(Space& home, unsigned int a, IntVar x, int i, int n) {
      if (a == 0) rel(home, x, IRT_EQ, n);
      else        rel(home, x, IRT_NQ, n);
    }

    int lower_bound_length() {
      int l = 0;
      for (int i = 0; i < boxes.size(); i++) {
        l = max(l, std::min(boxes[i].first, boxes[i].second));
      }
      return l;
    }

    Box(int _w, int _l, const vector<pair<int,int> >& _boxes) :
      w(_w), boxes(_boxes), length(*this, lower_bound_length(), _l),

      x_top(*this   , _boxes.size(), 0, _w-1),
      x_bottom(*this, _boxes.size(), 0, _w-1),
      y_top(*this   , _boxes.size(), 0,   _l),
      y_bottom(*this, _boxes.size(), 0,   _l)
    {
      for(int i = 0; i < boxes.size(); i++){

        // length constraint
        rel(*this, max(y_bottom)+1 == length);

        int width = boxes[i].first;
        int height = boxes[i].second;
        int dims[2] = {width, height};
        IntSet dim(dims, 2);
        IntVar v_width(*this, dim);
        IntVar v_height(*this, dim);
        // Required to enforce (w*h, h*w) combinations
        if(width != height) rel(*this, v_width != v_height);

        // margin constraint
        rel(*this, x_top[i] <= w-v_width);

        // bounding box constraint
        rel(*this,(x_bottom[i] == x_top[i]+v_width-1));
        rel(*this,(y_bottom[i] == y_top[i]+v_height-1));

        // overlapping constraint
        for(int j = i+1; j < boxes.size(); j++) {
          if (are_equal(boxes[i], boxes[j])) {
            // Breaking symmetries of identical boxes by imposng ordering.
            rel(*this, y_top[i] <= y_top[j]); // strict inequality here would be incorrect.
            rel(*this
               ,  (x_bottom[i] < x_top[j])
               || (y_bottom[i] < y_top[j])
               );
          }
          else {
            rel(*this
               ,  (x_top[j] > x_bottom[i])
               || (x_bottom[j] < x_top[i])
               || (y_top[j] > y_bottom[i])
               || (y_bottom[j] < y_top[i])
               );
          }
        }

        // Another way to exploit that there may be many identical boxes is to
        //   break symmetries by imposing an ordering on the coordinates they are assigned to.
        //
        // Notice, we can compare by pairs because we previously ordered the boxes.
        if (i > 0) {
          if (boxes[i-1].first == boxes[i].first && boxes[i-1].second == boxes[i].second) {
            // Notice that a strict inequality here would be incorrect.
            rel(*this, y_top[i-1] <= y_top[i]);
          }
        }

        // Symmetry
        if(i == 0) rel(*this, x_top[0] <= 1/2*(w - v_width));
      }

      // Explore horizontal solutions first.
      branch(*this, x_top, INT_VAR_NONE(), INT_VAL(&value, &commit));
      // Then, explore the vertical solutions then.
      branch(*this, y_top   , INT_VAR_NONE(), INT_VAL_MIN());
      branch(*this, y_bottom, INT_VAR_NONE(), INT_VAL_MIN());
    }

    Box(Box& b) : Space(b) {
      length.update(*this, b.length);

      x_top.update(*this,b.x_top);
      y_top.update(*this,b.y_top);
      x_bottom.update(*this,b.x_bottom);
      y_bottom.update(*this,b.y_bottom);

      w = b.w;
      boxes = b.boxes;
    }

    virtual Space* copy() {
      return new Box(*this);
    }

    void print(void) const {
      cout << length.val() << endl;
      for(int i = 0; i < x_top.size(); ++i) {
        cout << x_top[i].val() << " " << y_top[i].val()
             << "   "
             << x_bottom[i].val() << " " << y_bottom[i].val()
             << endl;
      }
    }

    // objective function
    virtual void constrain(const Space& s) {
      const Box& b = static_cast<const Box&>(s);
      rel(*this, length < b.length);
    }
};

int main(int argc, char* argv[]) {
  try {
    if (argc != 1) return -1;

    int w, n;
    int l = 0;
    cin >> w >> n;
    cout << w << " " << n << endl;
    vector<pair<int,int>> boxes(n);
    int k = 0;
    while (k < n) {
      // Parse the boxes
      int m, width, height;
      cin >> m >> width >> height;
      cout << m << "   " << width << " " << height << endl;
      for (int i = 0; i < m; ++i) {
        boxes[k+i] = pair<int,int>(width,height);
      }

      // Upper bounds of the length:
      //
      //     Compute a better maxLength by placing boxes with the same dimensions
      //       one next to the other until filling the width of the paper.
      //
      //     This gives better upper bounds than placing the boxes
      //       on above the other.
      //
      l += min( upper_bound_length(w, m, width, height)
              , upper_bound_length(w, m, height, width)
              );

      // Increase the number of boxes
      k += m;
    }

    // First-fail Principle: try placing the biggest bozxes first.
    sort(boxes.begin(), boxes.end(), cmp);

    Box* mod = new Box(w,l,boxes);
    BAB<Box> e(mod);
    delete mod;

    Box* sant = e.next();
    Box* s    = e.next();
    while (s != NULL) {
      delete sant;
      sant = s;
      s = e.next();
    }
    sant->print();
    delete sant;
  }
  catch (Exception e) {
    cerr << "Gecode exception: " << e.what() << endl;
    return 1;
  }
  return 0;
}
