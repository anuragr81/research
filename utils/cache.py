from functools import wraps
from inspect import getargspec

import numpy as np
import pandas as pd


class Store:
    """
    Class that wraps the store dictionary
    """
    def __init__(self):
        self.store_grid = {}

    def reset_grid(self):
        self.store_grid = {}


GRID_CACHE = Store()


def _equals(x, y):
    """
    A recursive function to compare any two arbitrary entities that relies on:
        i) type
        ii) len
        iii) repr
        iv) element-wise neq for dataframe elements
    """
    type_x = type(x)
    if type_x != type(y):
        return False
    elif type_x in [list, tuple]:
        return len(x) == len(y) and (not any(not _equals(xk, yk) for xk, yk in zip(x, y)))
    elif type_x == dict:
        return len(x) == len(y) and (not any(not _equals(x[k], y[k]) for k in x))
    elif type_x in [float, int, str, bool, complex]:
        return x == y
    elif type_x == pd.DataFrame:
        return x.shape != y.shape and not np.any(x != y)
    return repr(x) == repr(y)


def _get_full_args(f, args, kwargs):
    """
    Merges arguments and keyword arguments into a keyword arguments dictionary
    """
    # @TODO: use `getfullargspec` when moving to Python 3
    func_args = getargspec(f).args
    default_values = getargspec(f).defaults
    input_args = dict((func_args[i], args[i]) for i in range(0, len(args)))
    # Add provided optional arguments values
    for k, v in kwargs.items():
        if k in input_args:
            raise RuntimeError('Invalid Input')
        input_args.update({k: v})
    # Add defaulted optional argument values
    i = 0
    for arg in func_args:
        if arg not in input_args:
            input_args[arg] = default_values[i]
            i += 1

    return tuple(func_args), input_args


def _serialise(x):
    """
    A recursive serialisation function that relies on i) repr and ii) json for types that are not well-known
    """
    type_x = type(x)
    if type_x in [int, float, str]:
        return x
    elif type_x == dict:
        return repr(dict([(_serialise(k), _serialise(v)) for k, v in x.items()]))
    elif type_x == list:
        return repr([_serialise(k) for k in x])
    elif type_x == pd.DataFrame:
        return x.to_json()
    return repr(x)


def cached(f):
    """
    Function that acts as a decorator
    """
    @wraps(f)
    def wrapper(*args, **kwargs):

        func_args, input_args = _get_full_args(f, args, kwargs)
        args_tuple = tuple(_serialise(input_args[key]) for key in func_args)

        if f.__name__ not in GRID_CACHE.store_grid:
            GRID_CACHE.store_grid[f.__name__] = {}

        if args_tuple not in GRID_CACHE.store_grid[f.__name__]:
            GRID_CACHE.store_grid[f.__name__][args_tuple] = f(**input_args)

        return GRID_CACHE.store_grid[f.__name__][args_tuple]

    return wrapper



