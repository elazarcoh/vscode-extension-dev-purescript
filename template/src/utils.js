"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.log = exports.traced = exports.inputTraced = exports.curryThunk = exports.curry = void 0;
// export { curry } from 'lodash';
function curry(fn) {
    return function curried(...args) {
        if (args.length >= fn.length) {
            return fn.apply(this, args);
        }
        return function (...args2) {
            return curried.apply(this, args.concat(args2));
        };
    };
}
exports.curry = curry;
function curryThunk(fn, ...args) {
    return (...args2) => args2.length ? curryThunk(fn, ...args, ...args2) : fn(...args);
}
exports.curryThunk = curryThunk;
function inputTraced(fn) {
    return function (...args) {
        console.log(`${args.map(x => JSON.stringify(x)).join(', ')}`);
        return fn(...args);
    };
}
exports.inputTraced = inputTraced;
;
function traced(fn) {
    return function (...args) {
        return __awaiter(this, void 0, void 0, function* () {
            const val = yield fn(...args);
            console.log(val);
            return val;
        });
    };
}
exports.traced = traced;
;
function log(e) {
    console.log(e);
    return e;
}
exports.log = log;
;
