// export { curry } from 'lodash';
export function curry(fn) {
    return function curried(...args) {
        if (args.length >= fn.length) {
            return fn.apply(this, args);
        }
        return function (...args2) {
            return curried.apply(this, args.concat(args2));
        };
    }
}
export function curryThunk(fn: Function, ...args: any[]) {
    return (...args2: any[]) => args2.length ? curryThunk(fn, ...args, ...args2) : fn(...args);
}

export function traced(fn: Function) {
    return function (...args: any[]) {
        console.log(`${fn.name}(${args.map(x => JSON.stringify(x)).join(', ')})`);
        return fn(...args);
    };
};
export function log(e: any) {
    console.log(e);
    return e;
};

