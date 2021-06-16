"use strict";

// Note to reader: Add your solutions to this file
exports.volumeFn = function(a, b, c) {
    return a * b * c;
}

exports.volumeArrow = a => b => c => a * b * c;

exports.cumulativeSumsComplex = arr => {
    let sum = {
        real: 0,
        imag: 0
    };
    let sums = [];
    arr.forEach(x => {
        sum.real += x.real;
        sum.imag += x.imag;
        sums.push(Object.assign({}, sum));
    });
    return sums;
}

exports.quadraticRootsImpl = pair => quadratic => {
    const a = quadratic.a;
    const b = quadratic.b;
    const c = quadratic.c;
    const disc = (b*b) - (4*a*c);
    const isImag = disc < 0;
    const sq = Math.sqrt(Math.abs(disc));

    let first;
    let second;
    if(isImag) {
        first = {
            real: (-1 * b) / (2*a),
            imag: (-1 * sq) / (2*a)
        };
        second = {
            real: (-1 * b) / (2*a),
            imag: sq / (2*a)
        }
    }
    else {
        first = {
            real: ((-1 * b) + sq) / (2*a),
            imag: 0
        };
        second = {
            real: ((-1 * b) - sq) / (2*a),
            imag: 0
        }
    }

    return pair(first)(second);
}

exports.unsafeUndefinedValue = a => a;


exports.valuesOfMapJson = a => Array.from(new Set(new Map(a).values()));


exports.quadraticRootsSetJson = quadratic => {
    const { a, b , c } = quadratic;

    const disc = (b*b) - (4*a*c);

    let first;
    let second;
    if(disc < 0) {
        const sq = Math.sqrt(-disc);
        first = {
            real: -b / (2*a),
            imag: -sq / (2*a)
        };
        second = {
            real: (-1 * b) / (2*a),
            imag: sq / (2*a)
        }
    }
    else {
        const sq = Math.sqrt(disc);
        first = {
            real: (-b + sq) / (2*a),
            imag: 0
        };
        second = {
            real: (-b - sq) / (2*a),
            imag: 0
        }
    }

    return Array.from(new Set([first, second]));
}

exports.safeQuadraticRootsJson = quadratic => {
    const { a, b , c } = quadratic;

    const disc = (b*b) - (4*a*c);

    let first;
    let second;
    if(disc < 0) {
        const sq = Math.sqrt(-disc);
        first = {
            real: -b / (2*a),
            imag: -sq / (2*a)
        };
        second = {
            real: (-1 * b) / (2*a),
            imag: sq / (2*a)
        }
    }
    else {
        const sq = Math.sqrt(disc);
        first = {
            real: (-b + sq) / (2*a),
            imag: 0
        };
        second = {
            real: (-b - sq) / (2*a),
            imag: 0
        }
    }

    return [first, second];
}