unit jsonTools;

{$mode objfpc}{$H+}

interface

	uses
		simpleLinkedList,   
		doubleLinkedList    
	;

	function UploadUsersFromJson(const filePath: string): Boolean;
	function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;

	function UploadVehiclesFromJson(const filePath, userEmail: string): Boolean;
	function AddVehicleToJson(const filePath: string; const id: Integer; const marca, modelo, propietario, correo: string): Boolean;

implementation

	uses
		Classes, SysUtils, fpjson, jsonparser
	;

	// ---------------------------
	// Carga de usuarios desde JSON
	// ---------------------------
	function UploadUsersFromJson(const filePath: string): Boolean;
	var
		jsonData: TJSONData;
		jsonObject: TJSONObject;
		usersArray: TJSONArray;
		userItem: TJSONObject;
		i: Integer;
		fileStream: TFileStream;
	begin
		Result := False;

		if not FileExists(filePath) then
		begin
			Writeln('Archivo no encontrado: ', filePath);
			Exit;
		end;

		try
			fileStream := TFileStream.Create(filePath, fmOpenRead);
			try
				jsonData := GetJSON(fileStream);
			finally
				fileStream.Free;
			end;

			jsonObject := TJSONObject(jsonData);
			usersArray := jsonObject.Arrays['usuarios'];

			for i := 0 to usersArray.Count - 1 do
			begin
				userItem := usersArray.Objects[i];

				LSL_U_Insert(
					IntToStr(userItem.Integers['id']),
					userItem.Strings['nombre'],
					userItem.Strings['email'],
					userItem.Strings['usuario'],
					userItem.Strings['telefono'],
					userItem.Strings['password']
				);
			end;

			jsonData.Free;
			Result := True;

		except
			on E: Exception do
			begin
				Writeln('Error leyendo JSON: ', E.Message);
				Result := False;
			end;
		end;
	end;

	// ---------------------------
	// Añadir un usuario al JSON
	// ---------------------------
	function AddUserToJson(const filePath: string; const id: Integer; const name, email, username, phone, password: string): Boolean;
	var
		jsonData: TJSONData = nil;
		jsonObject: TJSONObject = nil;
		usersArray: TJSONArray = nil;
		newUser: TJSONObject;
		fileStream: TFileStream;
		jsonString: TStringList;
	begin
		Result := False;

		if FileExists(filePath) then
		begin
			try
				fileStream := TFileStream.Create(filePath, fmOpenRead);
				try
					jsonData := GetJSON(fileStream);
				finally
					fileStream.Free;
				end;

				jsonObject := TJSONObject(jsonData);
				usersArray := jsonObject.Arrays['usuarios'];

			except
				on E: Exception do
				begin
					Writeln('Error leyendo JSON existente: ', E.Message);
					Exit;
				end;
			end;
		end
		else
		begin
			// Si no existe, crear nuevo objeto JSON con array usuarios
			jsonObject := TJSONObject.Create;
			usersArray := TJSONArray.Create;
			jsonObject.Add('usuarios', usersArray);
		end;

		// Crear nuevo usuario
		newUser := TJSONObject.Create;
		newUser.Add('id', id);
		newUser.Add('nombre', name);
		newUser.Add('email', email);
		newUser.Add('usuario', username);
		newUser.Add('telefono', phone);
		newUser.Add('password', password);

		// Añadir nuevo usuario al array
		usersArray.Add(newUser);

		// Guardar JSON actualizado
		jsonString := TStringList.Create;
		try
			jsonString.Text := jsonObject.FormatJSON();
			jsonString.SaveToFile(filePath);
			Result := True;
		finally
			jsonString.Free;
			if Assigned(jsonData) then
				jsonData.Free
			else
				jsonObject.Free;
		end;
	end;

	// ---------------------------
	// Carga de vehículos desde JSON
	// ---------------------------
	function UploadVehiclesFromJson(const filePath, userEmail: string): Boolean;
		var
		jsonData: TJSONData;
		jsonObject: TJSONObject;
		vehiclesArray: TJSONArray;
		vehicleItem: TJSONObject;
		i: Integer;
		fileStream: TFileStream;
		begin
		Result := False;

		if not FileExists(filePath) then
		begin
			Writeln('Archivo no encontrado: ', filePath);
			Exit;
		end;

		try
			fileStream := TFileStream.Create(filePath, fmOpenRead);
			try
			jsonData := GetJSON(fileStream);
			finally
			fileStream.Free;
			end;

			jsonObject := TJSONObject(jsonData);
			vehiclesArray := jsonObject.Arrays['vehiculos'];

			for i := 0 to vehiclesArray.Count - 1 do
			begin
			vehicleItem := vehiclesArray.Objects[i];

			if SameText(vehicleItem.Strings['correo'], userEmail) then
			begin
				// Solo carga los vehículos con correo igual al userEmail
				LDE_V_Insert(
				IntToStr(vehicleItem.Integers['id']),
				vehicleItem.Strings['marca'],
				vehicleItem.Strings['modelo'],
				vehicleItem.Strings['correo']
				);
			end;
			end;

			jsonData.Free;
			Result := True;

		except
			on E: Exception do
			begin
			Writeln('Error leyendo JSON de vehículos: ', E.Message);
			Result := False;
			end;
		end;
	end;


	// ---------------------------
	// Añadir un vehículo al JSON
	// ---------------------------
	function AddVehicleToJson(const filePath: string; const id: Integer; const marca, modelo, propietario, correo: string): Boolean;
	var
		jsonData: TJSONData = nil;
		jsonObject: TJSONObject = nil;
		vehiclesArray: TJSONArray = nil;
		newVehicle: TJSONObject;
		fileStream: TFileStream;
		jsonString: TStringList;
	begin
		Result := False;

		if FileExists(filePath) then
		begin
			try
				fileStream := TFileStream.Create(filePath, fmOpenRead);
				try
					jsonData := GetJSON(fileStream);
				finally
					fileStream.Free;
				end;

				jsonObject := TJSONObject(jsonData);
				vehiclesArray := jsonObject.Arrays['vehiculos'];

			except
				on E: Exception do
				begin
					Writeln('Error leyendo JSON existente de vehículos: ', E.Message);
					Exit;
				end;
			end;
		end
		else
		begin
			// Si no existe, crear nuevo objeto JSON con array vehiculos
			jsonObject := TJSONObject.Create;
			vehiclesArray := TJSONArray.Create;
			jsonObject.Add('vehiculos', vehiclesArray);
		end;

		// Crear nuevo vehículo
		newVehicle := TJSONObject.Create;
		newVehicle.Add('id', id);
		newVehicle.Add('marca', marca);
		newVehicle.Add('modelo', modelo);
		newVehicle.Add('propietario', propietario);
		newVehicle.Add('correo', correo);

		// Añadir nuevo vehículo al array
		vehiclesArray.Add(newVehicle);

		// Guardar JSON actualizado
		jsonString := TStringList.Create;
		try
			jsonString.Text := jsonObject.FormatJSON();
			jsonString.SaveToFile(filePath);
			Result := True;
		finally
			jsonString.Free;
			if Assigned(jsonData) then
				jsonData.Free
			else
				jsonObject.Free;
		end;
	end;

end.
