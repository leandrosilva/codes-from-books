//===============================================================================
// Microsoft patterns & practices
// Parallel Programming Guide
//===============================================================================
// Copyright © Microsoft Corporation.  All rights reserved.
// This code released under the terms of the 
// Microsoft patterns & practices license (http://parallelpatterns.codeplex.com/license).
//===============================================================================

using System;
using System.Diagnostics;
using System.Windows.Input;

namespace Microsoft.Practices.ParallelGuideSamples.ADash.ViewModel
{
    /// <summary>
    /// A command object recognized by WPF. Implements the System.Windows.Input.ICommand interface.
    /// </summary>
    public class Command : ICommand
    {
        #region Fields

        readonly Action<object> execute;
        readonly Predicate<object> canExecute;
        EventHandler canExecuteChangedHandler;

        #endregion // Fields

        #region Constructors

        /// <summary>
        /// Creates a new command that can always execute.
        /// </summary>
        /// <param name="execute">The execution logic.</param>
        public Command(Action<object> execute)
            : this(execute, null)
        {
        }

        /// <summary>
        /// Creates a new command.
        /// </summary>
        /// <param name="execute">The execution logic.</param>
        /// <param name="canExecute">The execution status logic.</param>
        public Command(Action<object> executeAction, Predicate<object> canExecuteCommand)
        {
            if (executeAction == null)
                throw new ArgumentNullException("executeAction");

            execute = executeAction;
            canExecute = canExecuteCommand;
        }

        #endregion // Constructors

        #region ICommand Members

        /// <summary>
        /// Tests whether the current data context allows this command to be run
        /// </summary>
        /// <param name="parameter">Parameter passed to the object's CanExecute delegate</param>
        /// <returns>True if the command is currently enabled; false if not enabled.</returns>
        [DebuggerStepThrough]
        public bool CanExecute(object parameter)
        {
            return canExecute == null ? true : canExecute(parameter);
        }
        
        /// <summary>
        /// Event that is raised when the "CanExecute" status of this command changes
        /// </summary>
        public event EventHandler CanExecuteChanged
        {
            add 
            { 
                canExecuteChangedHandler = (EventHandler)Delegate.Combine(canExecuteChangedHandler, value);
                CommandManager.RequerySuggested += value;
            }

            remove 
            { 
                canExecuteChangedHandler = (EventHandler)Delegate.Remove(canExecuteChangedHandler, value);
                CommandManager.RequerySuggested -= value;
            }
        }        

        /// <summary>
        /// Performs the work of the "execute" delegate.
        /// </summary>
        /// <param name="parameter"></param>
        public void Execute(object parameter)
        {
            execute(parameter);
        }

        #endregion // ICommand Members

        /// <summary>
        /// Causes the CanExecuteChanged handler to run.
        /// </summary>
        /// <remarks>Should only be invoked by the view model</remarks>
        public void NotifyExecuteChanged()
        {
            EventHandler handler = canExecuteChangedHandler;
            if (handler != null)
                handler(this, EventArgs.Empty);
        }
    }
}