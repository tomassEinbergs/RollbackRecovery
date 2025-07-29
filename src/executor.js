// Ejecutor de sistema distribuido que implementa la lógica de recuperación por rollback
class Executor {
  constructor() {
    this.currentState = null;
    this.stepCounter = 0;
    this.parser = new FileParser();
  }

  // Inicializar sistema desde archivos de trazas
  async initializeFromTraces(traceFiles) {
    console.log('Inicializando sistema desde archivos de trazas:', traceFiles);
    
    // Comenzar solo con p1 (proceso principal)
    const p1Actions = await this.parser.parseTraceFile('p1.txt');
    
    const processes = [{
      id: 'p1',
      state: 's0',
      history: [],
      queue: p1Actions
    }];
    
    this.currentState = {
      processes: processes,
      messages: []
    };
    
    this.stepCounter = 0;
    
    console.log('Estado inicial creado:', this.currentState);
    return {
      success: true,
      state: this.currentState,
      step: this.stepCounter
    };
  }

  // Ejecutar siguiente paso (simulando exec)
  async executeNextStep() {
    if (!this.currentState) {
      return {
        success: false,
        error: 'No hay estado actual. Por favor inicialice primero.'
      };
    }

    // Encontrar un proceso que pueda ejecutar
    const executableProcess = this.findExecutableProcess();
    
    if (!executableProcess) {
      return {
        success: false,
        error: 'No hay más pasos posibles - ejecución terminada',
        isFinished: true
      };
    }

    // Ejecutar la siguiente acción
    const result = await this.executeAction(executableProcess);
    
    if (result.success) {
      this.stepCounter++;
      return {
        success: true,
        newState: this.currentState,
        executedAction: result.executedAction,
        isFinished: false,
        step: this.stepCounter
      };
    }

    return result;
  }

  // Encontrar un proceso que pueda ejecutar su siguiente acción
  findExecutableProcess() {
    for (const process of this.currentState.processes) {
      if (process.queue.length === 0) continue;
      
      const nextAction = process.queue[0];
      
      // Verificar si la acción puede ejecutarse
      if (this.canExecuteAction(process, nextAction)) {
        return process;
      }
    }
    return null;
  }

  // Verificar si una acción puede ejecutarse
  canExecuteAction(process, action) {
    if (action.startsWith('send(')) return true;
    if (action.startsWith('spawn(')) return true;
    if (action.startsWith('check(')) return true;
    if (action.startsWith('commit(')) return true;
    if (action.startsWith('rollback(')) return true;
    
    if (action.startsWith('receive(')) {
      // Verificar si hay un mensaje coincidente
      const tag = this.extractTag(action);
      return this.currentState.messages.some(msg => 
        msg.to === process.id && msg.tag === tag
      );
    }
    
    return false;
  }

  // Ejecutar acción (simulando reglas eval)
  async executeAction(process) {
    const action = process.queue[0];
    const actionType = action.split('(')[0];
    
    console.log(`Ejecutando ${process.id}: ${action}`);
    
    // Remover acción de la cola y agregar al historial
    process.queue.shift();
    process.history.push(action);
    
    switch (actionType) {
      case 'send':
        return this.executeSend(process, action);
      case 'receive':
        return this.executeReceive(process, action);
      case 'spawn':
        return await this.executeSpawn(process, action);
      case 'check':
        return this.executeCheck(process, action);
      case 'commit':
        return this.executeCommit(process, action);
      case 'rollback':
        return this.executeRollback(process, action);
      default:
        return { success: false, error: `Tipo de acción desconocido: ${actionType}` };
    }
  }

  // Ejecutar acción send
  executeSend(process, action) {
    const [dest, tag] = this.extractSendParams(action);
    
    // Agregar mensaje al tránsito
    this.currentState.messages.push({
      from: process.id,
      to: dest,
      tag: tag
    });
    
    return {
      success: true,
      executedAction: `${process.id}: ${action}`
    };
  }

  // Ejecutar acción receive
  executeReceive(process, action) {
    const tag = this.extractTag(action);
    
    // Encontrar y remover mensaje coincidente
    const messageIndex = this.currentState.messages.findIndex(msg => 
      msg.to === process.id && msg.tag === tag
    );
    
    if (messageIndex !== -1) {
      this.currentState.messages.splice(messageIndex, 1);
      process.state = tag; // Actualizar estado del proceso al tag recibido
      
      return {
        success: true,
        executedAction: `${process.id}: ${action}`
      };
    }
    
    return { success: false, error: `No hay mensaje coincidente para ${action}` };
  }

  // Ejecutar acción spawn
  async executeSpawn(process, action) {
    const childId = this.extractTag(action);
    
    // Verificar si el hijo ya existe
    if (this.currentState.processes.find(p => p.id === childId)) {
      return { success: false, error: `El proceso ${childId} ya existe` };
    }
    
    // Parsear el archivo de trazas del hijo
    const childActions = await this.parser.parseTraceFile(`${childId}.txt`);
    console.log(`>>> Generando ${childId} con acciones:`, childActions);
    
    // Crear nuevo proceso hijo
    const childProcess = {
      id: childId,
      state: 's0',
      history: [`spawned_by(${process.id})`],
      queue: childActions
    };
    
    this.currentState.processes.push(childProcess);
    
    return {
      success: true,
      executedAction: `${process.id}: ${action} - creado ${childId}`
    };
  }

  // Ejecutar acción checkpoint
  executeCheck(process, action) {
    // Checkpoint - solo registrarlo
    return {
      success: true,
      executedAction: `${process.id}: ${action}`
    };
  }

  // Ejecutar acción commit
  executeCommit(process, action) {
    const checkId = this.extractTag(action);
    
    // Limpiar historial hasta checkpoint
    const newHistory = [];
    let foundCheckpoint = false;
    
    // Mantener spawned_by y todo después del checkpoint
    for (let i = process.history.length - 1; i >= 0; i--) {
      const histAction = process.history[i];
      if (histAction === `check(${checkId})`) {
        foundCheckpoint = true;
        newHistory.unshift(histAction);
        break;
      }
      newHistory.unshift(histAction);
    }
    
    // Agregar de vuelta cualquier acción spawned_by
    const spawnedByActions = process.history.filter(h => h.startsWith('spawned_by('));
    process.history = [...spawnedByActions, ...newHistory];
    
    return {
      success: true,
      executedAction: `${process.id}: ${action} - cleaned history`
    };
  }

  // Ejecutar acción rollback
  executeRollback(process, action) {
    const checkId = this.extractTag(action);
    
    console.log(`>>> Rollback a ${checkId} iniciado por ${process.id}`);
    
    // Encontrar checkpoint y obtener acciones deshechas
    const rollbackResult = this.rollbackToCheckpoint(checkId, process.history);
    
    if (rollbackResult) {
      const { newHistory, undoneActions } = rollbackResult;
      
      console.log(`>>> Hist: ${process.history}`);
      console.log(`>>> NewHist: ${newHistory}`);
      console.log(`>>> Undone: ${undoneActions}`);
      
      // Actualizar historial del proceso
      process.history = newHistory;
      
      // Reiniciar estado
      process.state = 's0';
      
      // Procesar acciones deshechas paso a paso con cascada
      this.processUndoneActionsStepByStep(undoneActions, process.id);
      
      // Agregar acciones deshechas de vuelta a la cola en orden correcto (acción deshecha más reciente primero)
      // Las acciones se restauran en orden cronológico normal a la cola
      // No agregar la acción rollback misma de vuelta a la cola
      const actionsToRestore = undoneActions.filter(action => !action.startsWith('rollback('));
      process.queue.unshift(...actionsToRestore);
      
      return {
        success: true,
        executedAction: `${process.id}: ${action} - rollback a ${checkId}`
      };
    }
    
    return {
      success: true,
      executedAction: `${process.id}: ${action} - rollback al inicio`
    };
  }

  // Rollback a checkpoint
  rollbackToCheckpoint(checkId, history) {
    // Invertir historial para empezar con la acción más reciente
    const revHistory = [...history].reverse();
    
    // Encontrar posición del checkpoint
    const checkIndex = revHistory.findIndex(action => action === `check(${checkId})`);
    
    if (checkIndex === -1) {
      // No se encontró checkpoint, rollback al inicio
      const spawnedByActions = history.filter(h => h.startsWith('spawned_by('));
      return {
        newHistory: spawnedByActions,
        undoneActions: history.filter(h => !h.startsWith('spawned_by('))
      };
    }
    
    // Dividir en el checkpoint
    const undoneActions = revHistory.slice(0, checkIndex);
    const keptActions = revHistory.slice(checkIndex);
    
    return {
      newHistory: [...keptActions].reverse(),
      undoneActions: [...undoneActions].reverse()
    };
  }

  // Procesar acciones deshechas paso a paso
  processUndoneActionsStepByStep(undoneActions, initiatorId) {
    console.log(`>>> Procesando acciones deshechas paso a paso: ${undoneActions}`);
    
    // Procesar acciones en orden cronológico INVERSO (LIFO)
    // Las acciones más recientes deben deshacerse primero
    for (let i = undoneActions.length - 1; i >= 0; i--) {
      const action = undoneActions[i];
      console.log(`>>> Procesando acción deshecha: ${action}`);
      
      if (action.startsWith('receive(')) {
        this.processUndoneReceive(action, initiatorId);
      } else if (action.startsWith('send(')) {
        this.processUndoneSend(action, initiatorId);
      } else if (action.startsWith('spawn(')) {
        this.processUndoneSpawn(action, initiatorId);
      } else if (action.startsWith('check(')) {
        this.processUndoneCheck(action, initiatorId);
      }
    }
  }
  
  // Procesar receive deshecho: poner mensaje de vuelta en cola de tránsito
  processUndoneReceive(action, initiatorId) {
    const tag = this.extractTag(action);
    console.log(`>>> Deshaciendo receive(${tag}) - restaurando mensaje al tránsito`);
    
    // Encontrar quién envió originalmente este mensaje
    const sender = this.findOriginalSender(tag, initiatorId);
    if (sender) {
      console.log(`>>> Restaurando mensaje ${sender} -> ${initiatorId} :: ${tag}`);
      this.currentState.messages.push({
        from: sender,
        to: initiatorId,
        tag: tag
      });
    }
  }
  
  // Procesar send deshecho: remover del tránsito O propagar rollback si fue recibido
  processUndoneSend(action, initiatorId) {
    const [dest, tag] = this.extractSendParams(action);
    console.log(`>>> Deshaciendo send(${dest}, ${tag})`);
    
    // Verificar si el mensaje fue recibido por el destino
    const destProcess = this.currentState.processes.find(p => p.id === dest);
    if (destProcess && destProcess.history.some(h => h === `receive(${tag})`)) {
      console.log(`>>> Mensaje ${tag} fue recibido por ${dest} - propagando rollback`);
      this.propagateRollbackToProcess(dest, tag);
    } else {
      console.log(`>>> Mensaje ${tag} no recibido por ${dest} - removiendo del tránsito`);
      // Remover mensaje del tránsito
      this.currentState.messages = this.currentState.messages.filter(
        msg => !(msg.from === initiatorId && msg.to === dest && msg.tag === tag)
      );
    }
  }
  
  // Procesar spawn deshecho: rollback completo del hijo y luego removerlo
  processUndoneSpawn(action, initiatorId) {
    const childId = this.extractTag(action);
    console.log(`>>> Deshaciendo spawn(${childId}) - rollback del hijo y luego remover`);
    
    const childProcess = this.currentState.processes.find(p => p.id === childId);
    if (childProcess) {
      // Obtener todas las acciones excepto spawned_by
      const childUndoneActions = childProcess.history.filter(h => !h.startsWith('spawned_by('));
      
      if (childUndoneActions.length > 0) {
        console.log(`>>> Rollback de acciones del hijo ${childId}: ${childUndoneActions}`);
        // Procesar acciones deshechas del hijo recursivamente
        this.processUndoneActionsStepByStep(childUndoneActions, childId);
      }
      
      // Remover proceso hijo
      console.log(`>>> Removiendo proceso hijo ${childId}`);
      
      // Remover cualquier mensaje que involucre al hijo
      const messagesToRemove = this.currentState.messages.filter(
        msg => msg.from === childId || msg.to === childId
      );
      
      if (messagesToRemove.length > 0) {
        console.log(`>>> Removiendo ${messagesToRemove.length} mensaje(s) que involucran a ${childId}:`);
        messagesToRemove.forEach(msg => {
          console.log(`>>>   - ${msg.from} -> ${msg.to} :: ${msg.tag}`);
        });
      }
      
      this.currentState.processes = this.currentState.processes.filter(p => p.id !== childId);
      this.currentState.messages = this.currentState.messages.filter(
        msg => msg.from !== childId && msg.to !== childId
      );
    }
  }
  
  // Procesar check deshecho
  processUndoneCheck(action, initiatorId) {
    console.log(`>>> Deshaciendo ${action} - no se necesita acción especial`);
  }
  
  // Propagar rollback a un proceso específico
  propagateRollbackToProcess(processId, triggerTag) {
    const targetProcess = this.currentState.processes.find(p => p.id === processId);
    if (!targetProcess) {
      console.log(`>>> Proceso ${processId} no encontrado para propagación de rollback`);
      return;
    }
    
    console.log(`>>> Propagando rollback a ${processId} debido al tag ${triggerTag}`);
    
    // Encontrar checkpoint dependiente
    const checkId = this.findDependentCheckpoint(targetProcess.history, triggerTag);
    
    console.log(`>>> Checkpoint dependiente encontrado: ${checkId}`);
    
    if (checkId === 'tau0') {
      // Rollback al inicio (mantener solo spawned_by)
      const spawnedByActions = targetProcess.history.filter(h => h.startsWith('spawned_by('));
      const undoneActions = targetProcess.history.filter(h => !h.startsWith('spawned_by('));
      
      targetProcess.history = spawnedByActions;
      targetProcess.state = 's0';
      
      // Procesar acciones deshechas
      if (undoneActions.length > 0) {
        this.processUndoneActionsStepByStep(undoneActions, processId);
        // Agregar acciones deshechas de vuelta a la cola
        targetProcess.queue.unshift(...undoneActions);
      }
    } else {
      // Rollback a checkpoint específico
      const rollbackResult = this.rollbackToCheckpoint(checkId, targetProcess.history);
      if (rollbackResult) {
        targetProcess.history = rollbackResult.newHistory;
        targetProcess.state = 's0';
        
        // Procesar acciones deshechas
        if (rollbackResult.undoneActions.length > 0) {
          this.processUndoneActionsStepByStep(rollbackResult.undoneActions, processId);
          // Agregar acciones deshechas de vuelta a la cola
          targetProcess.queue.unshift(...rollbackResult.undoneActions);
        }
      }
    }
  }

  // Encontrar checkpoint dependiente
  findDependentCheckpoint(history, tag) {
    const revHistory = [...history].reverse();
    
    // Encontrar posición de receive(tag)
    const receiveIndex = revHistory.findIndex(action => action === `receive(${tag})`);
    
    if (receiveIndex !== -1) {
      for (let i = receiveIndex + 1; i < revHistory.length; i++) {
        if (revHistory[i].startsWith('check(')) {
          return this.extractTag(revHistory[i]);
        }
      }
    }
    
    return 'tau0'; // No se encontró checkpoint
  }

  // Encontrar remitente original de un mensaje (para rollback de receive)
  findOriginalSender(tag, receiverId) {
    for (const process of this.currentState.processes) {
      if (process.id !== receiverId && 
          process.history.some(h => h.startsWith(`send(${receiverId},${tag})`))) {
        return process.id;
      }
    }
    return null;
  }

  // Métodos auxiliares
  extractTag(action) {
    const match = action.match(/\(([^)]+)\)/);
    return match ? match[1] : '';
  }

  extractSendParams(action) {
    const match = action.match(/send\(([^,]+),\s*([^)]+)\)/);
    return match ? [match[1], match[2]] : ['', ''];
  }

  reset() {
    this.currentState = null;
    this.stepCounter = 0;
    return { success: true, message: 'Sistema reiniciado' };
  }
}
