import numpy as np

from mmp_config import model


def model_ext(size, npts, etop, **kw):
    m = model(size, ne=npts, nl=npts, **kw)
    h = 0.4 / (npts - 1)
    k = int(round((etop - 0.4) / (5 * h)))
    m["eg"] = np.concatenate([np.linspace(0.0, 0.4, npts), np.linspace(0.4, etop, k + 1)[1:]])
    return m
