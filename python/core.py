import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

lambda_limit = 1000

def transpose(matrix):
    return list(map(list, zip(*matrix)))

def fp(lmd, k):
    return min(1, np.exp(-((k - lmd) ** 2) / (2 * lmd)) / np.sqrt(2 * np.pi * lmd))

def normal_fp(lmd, k):
    return norm.pdf(k, loc=lmd, scale=np.sqrt(lmd))

def cf(x):
    return int(np.ceil(x))

def gxf(a, b, x):
    return np.exp(a * np.log(x) + b)

def nf(x):
    return int(gxf(-0.24, 8.42, x))

def stepf(x):
    return gxf(0.41, -2.51, x)

def gxf_chart(f, name):
    x = np.arange(1, 10000)
    y = [f(float(i)) for i in x]
    plt.figure()
    plt.plot(x, y)
    plt.xlabel('lambda')
    plt.ylabel(name)
    plt.title(f'Chart of {name}')
    plt.show()

def gxf_charts():
    gxf_chart(nf, "N")
    gxf_chart(stepf, "step")

def get_fks(lmd):
    if lmd > lambda_limit:
        return (normal_fp, 
                np.arange(0, int(lmd + 6 * np.sqrt(lmd)), max(1, int(np.floor(np.sqrt(lmd / lambda_limit))))))
    else:
        step = stepf(lmd)
        return (fp, [k * step for k in range(nf(lmd))])

def fps(lmd):
    f, ks = get_fks(lmd)
    return [(k, f(lmd, k)) for k in ks]

def sparse_rounded_fps(lmd):
    acc = []
    for k, p in fps(lmd):
        if acc:
            pd, pck, _ = acc[-1]
        else:
            pd, pck = None, None
        cck = cf(k)
        cd = cck - k
        if pd is not None:
            if pck < cck:
                cd_ = k - pck
                if cd_ < pd:
                    acc[-1] = [cd_, pck, p]
                    acc.append([cd, cck, p])
                else:
                    acc.append([cd, cck, p])
            else:
                acc[-1] = [cd, cck, p]
        else:
            acc.append([cd, cck, p])
    return [(item[1], item[2]) for item in acc if item[0] > 0]

def find_threshold(lmd, target_prob):
    rb = (1 - target_prob) * 0.1
    cumulative_prob = 0.0
    kps = sparse_rounded_fps(lmd)
    for k, p in kps:
        if cumulative_prob >= target_prob or (cumulative_prob > p * 10 and p < rb) or (cumulative_prob > 0 and p == 0):
            return k
        cumulative_prob += p
    return kps[-1][0]

def normal_quantile(lmd, target_prob):
    z = 6 if target_prob > 0.999999999 else norm.ppf(target_prob)
    return int(np.ceil(lmd + z * np.sqrt(lmd)))

def peak_load_estimation(lmd, target_prob):
    if lmd > lambda_limit:
        return normal_quantile(lmd, target_prob)
    return find_threshold(lmd, target_prob)

def limit_prob(prob, kps):
    min_p = 1 - prob
    return [kp for kp in kps if kp[1] > min_p]

def add_filled_area(plot, highlight_x, highlight_y):
    plot.fill_between(highlight_x, highlight_y, color='blue', alpha=0.3)
    plot.plot(highlight_x, highlight_y, color='blue', linewidth=2, label='Target probability')

def chart(lmd, title, opts=None):
    if opts is None:
        opts = {}
    x_coef = opts.get('x_coef', 1)
    prob = opts.get('prob', 0.99999999)
    threshold = opts.get('threshold', None)
    if threshold:
        threshold *= x_coef

    kps = limit_prob(prob, fps(lmd))
    x = [kp[0] * x_coef for kp in kps]
    y = [kp[1] for kp in kps]
    title = f'Peak per {title}: {threshold}' if threshold else title

    plt.figure()
    plt.plot(x, y, color='red', label='Poisson distribution')
    plt.xlabel('N')
    plt.ylabel('P')
    plt.title(title)

    if threshold:
        area_x = [xi for xi in x if xi <= threshold]
        area_y = y[:len(area_x)]
        add_filled_area(plt, area_x, area_y)
    
    plt.legend()
    plt.show(block=False)
    plt.pause(0.1)

def triples(day_n, opts=None):
    if opts is None:
        opts = {}
    grow = opts.get('grow', 0)
    prob = opts.get('prob', 0.95)
    total = day_n * (1 + grow)
    aph = (day_n * 0.1383) / 2  # 13.83% of all requests come in interval between 13:30 and 15:30 (2 hours)
    thh = peak_load_estimation(aph, prob)
    apm = thh / 60
    thm = peak_load_estimation(apm, prob)
    aps = thm / 60
    ths = peak_load_estimation(aps, prob)

    if opts.get('chart', False):
        chart(aph, 'Hour', {'threshold': thh})
        chart(apm, 'Minute', {'threshold': thm})
        chart(aps, 'Second', {'threshold': ths})
        plt.show()

    return {
        'prob': prob,
        'total': total,
        'average': {
            'ph': total / 24.0,
            'pm': total / (24 * 60.0),
            'ps': total / (24 * 60 * 60.0)
        },
        'peak': {
            'ph': thh,
            'pm': thm,
            'ps': ths
        }
    }

def __main__():
    # Example usage:
    # print(triples(3200, {'grow': 0.5}))

    print(triples(22050, {'prob': 0.9995, 'chart': True}))
    print(triples(2887500, {'prob': 0.9995, 'chart': True}))

__main__()