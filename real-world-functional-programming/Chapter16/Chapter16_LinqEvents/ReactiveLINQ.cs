using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.Windows.Forms;
using System.Diagnostics;

namespace FunctionalCSharp
{
  // IObserver & IObservable methods that will be available in .NET 4.0

  public interface IObservable<T>
  {
    IDisposable Subscribe(IObserver<T> observer);
  }

  public interface IObserver<T>
  {
    void OnCompleted();
    void OnError(Exception exception);
    void OnNext(T value);
  }

  // Private - observable implementation

  class Disposable : IDisposable
  {
    Action dispose;

    public Disposable(Action dispose)
    {
      this.dispose = dispose;
    }

    public void Dispose()
    {
      dispose();
    }
  }

  class AttachedObservable<T> : IObservable<T>
  {
    EventInfo ei;
    object owner;
    MethodInfo closureInvoke;

    public AttachedObservable(object owner, string name)
    {
      closureInvoke = typeof(Closure).GetMethod("Invoke", BindingFlags.Public | BindingFlags.Instance);
      ei = owner.GetType().GetEvent(name);
      this.owner = owner;
    }

    class Closure
    {
      IObserver<T> observer;

      public Closure(IObserver<T> f)
      {
        this.observer = f;
      }

      public void Invoke(object sender, T e)
      {
        observer.OnNext(e);
      }
    }

    public IDisposable  Subscribe(IObserver<T> observer)
    {
      var eh = Delegate.CreateDelegate(ei.EventHandlerType, new Closure(observer), closureInvoke);
      ei.AddEventHandler(owner, eh);
      return new Disposable(() => ei.RemoveEventHandler(owner, eh));
    }
  }

  class ConcreteObservable<T> : IObservable<T>
  {
    Func<IObserver<T>, IDisposable> subscribe;

    public ConcreteObservable(Func<IObserver<T>, IDisposable> subscribe)
    {
      this.subscribe = subscribe;
    }

    public IDisposable Subscribe(IObserver<T> observer)
    {
      return subscribe(observer);
    }
  }

  class ConcreteObserver<T> : IObserver<T>
  {
    Action completed;
    Action<Exception> error;
    Action<T> next;

    public ConcreteObserver(Action completed, Action<Exception> error, Action<T> next) {
      this.completed = completed;
      this.error = error;
      this.next = next;
    }

    public void OnCompleted()
    {
      completed();
    }

    public void OnError(Exception exception)
    {
      error(exception);
    }

    public void OnNext(T value)
    {
      next(value);
    }
  }

  // Attaches IObservable to Events
  
  public static class EventUtils
  {
    public static IObservable<EventArgs> AttachEvent(this Control owner, string name)
    {
      return new AttachedObservable<EventArgs>(owner, name);
    }

    public static IObservable<T> AttachEvent<T>(this Control owner, string name)
    {
      return new AttachedObservable<T>(owner, name);
    }
  }

  public static class Observable
  {
    // CREATE etc.

    public static IObservable<EventArgs> Attach(object owner, string name)
    {
      return new AttachedObservable<EventArgs>(owner, name);
    }

    public static IObservable<T> Attach<T>(object owner, string name)
    {
      return new AttachedObservable<T>(owner, name);
    }

    // FUNCTIONS ..

    public static IObservable<T> Merge<T>(IObservable<T> a, IObservable<T> b)
    {
      return new ConcreteObservable<T>(observer => {
        var bothRunning = true;
        var adisp = a.Subscribe(new ConcreteObserver<T>(
            () => { if (bothRunning) { bothRunning = false; } else observer.OnCompleted(); },
            observer.OnError,
            observer.OnNext));
        var bdisp = b.Subscribe(new ConcreteObserver<T>(
            () => { if (bothRunning) { bothRunning = false; } else observer.OnCompleted(); },
            observer.OnError,
            observer.OnNext));
        return new Disposable(() =>
        {
          adisp.Dispose();
          bdisp.Dispose();
        });
      });
    }

    public static void Add<T>(this IObservable<T> ev, Action<T> handler)
    {
      ev.Subscribe(new ConcreteObserver<T>(() => { }, err => { }, handler));
    }

    // LINQ query operators

    public static IObservable<TResult> Select<T, TResult>(this IObservable<T> ev, Func<T, TResult> mapf)
    {
      return new ConcreteObservable<TResult>(observer => 
        ev.Subscribe(new ConcreteObserver<T>(
            observer.OnCompleted, observer.OnError,
            v => observer.OnNext(mapf(v)))));
    }

    public static IObservable<T> Where<T>(this IObservable<T> ev, Func<T, bool> filterf)
    {
      return new ConcreteObservable<T>(observer =>
        ev.Subscribe(new ConcreteObserver<T>(
            observer.OnCompleted, observer.OnError,
            v => { if (filterf(v)) observer.OnNext(v); })));
    }

    public static IObservable<TAcc> Aggregate<T, TAcc>(this IObservable<T> source, TAcc seed, Func<TAcc, T, TAcc> func)
    {
      return new ConcreteObservable<TAcc>(observer =>
      {
        var state = seed;
        return source.Subscribe(new ConcreteObserver<T>(
            observer.OnCompleted, observer.OnError,
            v => { state = func(state, v); observer.OnNext(state); }));
      });
    }

    public static IObservable<int> Sum<TSource>(this IObservable<TSource> source, Func<TSource, int> selector)
    {
      return source.Aggregate(0, (a, b) => a + selector(b));
    }

    public static IObservable<double> Sum<TSource>(this IObservable<TSource> source, Func<TSource, double> selector)
    {
      return source.Aggregate(0.0, (a, b) => a + selector(b));
    }

    public static IObservable<int> Sum(this IObservable<int> source)
    {
      return source.Aggregate(0, (a, b) => a + b);
    }

    public static IObservable<double> Sum(this IObservable<double> source)
    {
      return source.Aggregate(0.0, (a, b) => a + b);
    }
  } 
}
