#include <cmath>

// Forward declarations for Path class
class Path;

// Individual class representing individuals
class Individual {
public:
    Path w;
    double x, y, z;
};

// Declaration for Mind class containing member functions from the 'mind' module
class Mind {
public:
    void makeI(Path w, double x, double y, double z, Individual& i);
    void pathI(const Individual& i, Path& w);
    double xposI(const Individual& i);
    double yposI(const Individual& i);
    double zposI(const Individual& i);
    double distI(const Individual& i1, const Individual& i2);
    bool ind_eqI(const Individual& i1, const Individual& i2);
};

// Implementation of the functions in the Mind class

void Mind::makeI(Path w, double x, double y, double z, Individual& i) {
    i.w = w;
    i.x = x;
    i.y = y;
    i.z = z;
}

void Mind::pathI(const Individual& i, Path& w) {
    w = i.w;
}

double Mind::xposI(const Individual& i) {
    return i.x;
}

double Mind::yposI(const Individual& i) {
    return i.y;
}

double Mind::zposI(const Individual& i) {
    return i.z;
}

double Mind::distI(const Individual& i1, const Individual& i2) {
    double a = i1.x;
    double b = i1.y;
    double c = i1.z;
    double d = i2.x;
    double e = i2.y;
    double f = i2.z;

    return sqrt(((a - d) * (a - d)) + ((b - e) * (b - e)) + ((c - f) * (c - f)));
}

bool Mind::ind_eqI(const Individual& i1, const Individual& i2) {
    return (xposI(i1) == xposI(i2)) && (yposI(i1) == yposI(i2)) && (zposI(i1) == zposI(i2));
}
