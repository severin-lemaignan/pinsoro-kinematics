from matplotlib import pyplot as plt
import numpy as np
import pandas as pd
from scipy import stats
import matplotlib
matplotlib.rcParams['figure.figsize'] = (20.0, 10.0) # bigger figure!

from mpl_toolkits.mplot3d import Axes3D
from matplotlib.font_manager import FontProperties
import seaborn as sns

sns.set()

fontP = FontProperties()
fontP.set_size('small')



# helper function for table-wise heatmap
def background_gradient(s, m, M, cmap='PuBu', low=0, high=0):
    rng = M - m
    norm = matplotlib.colors.Normalize(m - (rng * low),
                                       M + (rng * high))
    normed = norm(s.values)
    c = [matplotlib.colors.rgb2hex(x) for x in plt.cm.get_cmap(cmap)(normed)]
    return ['background-color: %s' % color if not np.isnan(normed[i]) else 'background-color: #fff;color: #fff' for i, color in enumerate(c)]


def show_heatmap(df, m=None, M=None, cmap='PuOr_r'):
    return df.style.apply(background_gradient,
                          cmap=cmap,
                          m=df.min().min() if m is None else m,
                          M=df.max().max() if M is None else M,
                          low=0,
                          high=0.2)

def plot_embedding(X, labels, means=None, means_labels=None, clusters=None, title=None, three_d=False):

    markers = ["o","x","s","v", "*", "^",">","<","8","p","D"]

    plt.figure()

    labels = np.array(labels) # needed for masking with plotting with clusters
    colors = np.array([list(set(labels)).index(l) for l in labels])

    if means_labels is not None:
        colors_means = [sns.color_palette("tab20",20)[list(set(labels)).index(l)] for l in means_labels]

    handles=[]
    if three_d:
        ax = plt.subplot(111, projection='3d')
        if clusters is not None:
            for cluster in set(clusters):
                mask = clusters == cluster
                ax.scatter(X[mask, 0], X[mask, 1], X[mask,2],s=200, marker=markers[cluster], c=colors[mask], label=labels[mask], cmap=plt.get_cmap("tab20"))
        else:
            ax.scatter(X[:, 0], X[:, 1], X[:,2], marker="o",s=200, c=colors, label=labels, cmap=plt.get_cmap("tab20"))

        ax.set_zlabel('3rd component')

        if means is not None:
            for i in range(means.shape[0]):
                points=ax.scatter(means[i, 0], means[i, 1], means[i,2], marker="o", s=200, c=colors_means[i], label=means_labels[i])
                handles.append(points)

    else:
        ax = plt.subplot(111)
        if clusters is not None:
            for cluster in set(clusters):
                mask = clusters == cluster
                ax.scatter(X[mask, 0], X[mask, 1], marker=markers[cluster], s=100, c=colors[mask], label=labels[mask], cmap=plt.get_cmap("tab20"))
        else:
            ax.scatter(X[:, 0], X[:, 1], marker="o", c=colors[:], label=labels[:], cmap=plt.get_cmap("tab20"))

        if means is not None:
            for i in range(means.shape[0]):
                points=ax.scatter(means[i, 0], means[i, 1], marker="o", s=200, c=colors_means[i], label=means_labels[i])
                handles.append(points)


    ax.set_xlabel('1st component')
    ax.set_ylabel('2nd component')
    if title is not None:
        ax.set_title(title)

    if means is not None:
        ax.legend(handles, means_labels, loc=1, prop=fontP)

def plot_compare_embeddings(X, Y, labels, title=None, three_d=False):

    plt.figure()
    labels = np.array(labels) # needed for masking with plotting with clusters

    colors = np.array([sns.color_palette("tab20",20)[list(labels).index(l)] for l in labels])

    handles=[]

    if three_d:
        ax = plt.subplot(111, projection='3d')

        for i in range(X.shape[0]):
            points=ax.scatter(X[i, 0], X[i, 1], X[i,2], marker="o", s=200, c=colors[i], label=labels[i])
            handles.append(points)
            points=ax.scatter(Y[i, 0], Y[i, 1], Y[i,2], marker="x", s=200, c=colors[i], label=labels[i])

            ax.plot((X[i,0], Y[i,0]), (X[i,1], Y[i,1]), (X[i,2], Y[i,2]), '-',c=colors[i])

        ax.set_zlabel('3rd component')
        #ax.set_zlim(-3,3)

    else:
        ax = plt.subplot(111)

        for i in range(X.shape[0]):
            points=ax.scatter(X[i, 0], X[i, 1], marker="o", s=200, c=colors[i], label=labels[i])
            handles.append(points)
            ax.scatter(Y[i, 0], Y[i, 1], marker="x", s=200, c=colors[i], label=labels[i])

            ax.plot((X[i,0], Y[i,0]), (X[i,1], Y[i,1]), '-',c=colors[i])


    ax.set_xlabel('1st component')
    ax.set_ylabel('2nd component')

    #ax.set_xlim(-3,3)
    #ax.set_ylim(-3,3)


    ax.legend(handles, labels, loc='best', prop=fontP)
    ax.set_title("O: skel (skeletons), X: fullscene (full scene)")
    if title is not None:
        ax.set_title(title)


import itertools
def plot_confusion_matrix(cm, classes=[],
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    plt.figure()
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    #print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=90)
    plt.yticks(tick_marks, classes)

    fmt = '.1f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
